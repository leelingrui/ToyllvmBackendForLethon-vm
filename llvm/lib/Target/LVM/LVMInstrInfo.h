#ifndef LLVM_LIB_TARGET_LVM_LVMINSTRINFO_H
#define LLVM_LIB_TARGET_LVM_LVMINSTRINFO_H

#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/ISDOpcodes.h"
#include "LVMAnalyzeImmediate.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "LVMRegisterInfo.h"

#define GET_INSTRINFO_HEADER
#include "LVMGenInstrInfo.inc"

namespace llvm {

    class LVMSubtarget;

    class LVMInstrInfo final : public LVMGenInstrInfo
    {
    private:
        const LVMRegisterInfo RI;
        const LVMSubtarget &STI;
        virtual void anchor();
    public:
        void loadImmediate(int64_t Imm, MCPhysReg targetReg, MachineBasicBlock &MBB, MachineBasicBlock::iterator II, const DebugLoc &DL, unsigned *NewImm) const;
        unsigned GetInstSizeInBytes(const MachineInstr &MI) const;
        LVMInstrInfo(const LVMSubtarget &STI);
        void storeRegToStackSlot(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI, Register SrcReg, bool isKill, int FrameIndex, \
                           const TargetRegisterClass *RC, const TargetRegisterInfo *TRI) const override 
        {
            storeRegToStack(MBB, MBBI, SrcReg, isKill, FrameIndex, RC, TRI, 0);
        }
        void loadRegFromStackSlot(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI, Register DestReg, int FrameIndex,
                            const TargetRegisterClass *RC, const TargetRegisterInfo *TRI) const override 
        {
            loadRegFromStack(MBB, MBBI, DestReg, FrameIndex, RC, TRI, 0);
        }
        virtual void storeRegToStack(MachineBasicBlock &MBB, MachineBasicBlock::iterator MI, unsigned SrcReg, bool isKill, int FrameIndex, \
                               const TargetRegisterClass *RC, const TargetRegisterInfo *TRI, int64_t Offset) const;
        virtual void loadRegFromStack(MachineBasicBlock &MBB, MachineBasicBlock::iterator MI, unsigned DestReg, int FrameIndex, \
                                const TargetRegisterClass *RC, const TargetRegisterInfo *TRI, int64_t Offset) const;
        explicit LVMInstrInfo(LVMSubtarget &STI);
        virtual void adjustStackPtr(unsigned SP, int64_t Amount, MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator I) const;
        void copyPhysReg(MachineBasicBlock &MBB, MachineBasicBlock::iterator MI, const DebugLoc &DL, MCRegister DestReg, MCRegister SrcReg, bool KillSrc) const override;
    protected:
        MachineMemOperand *GetMemOperand(MachineBasicBlock &MBB, int FI, MachineMemOperand::Flags Flags) const;
        
    };

} // end namespace llvm

#endif // LLVM_LIB_TARGET_LVM_LVMINSTRINFO_H