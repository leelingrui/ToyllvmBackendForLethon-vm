#ifndef LLVM_LIB_TARGET_LVM_LVMREGISTERINFO_H
#define LLVM_LIB_TARGET_LVM_LVMREGISTERINFO_H
#include "llvm/CodeGen/TargetRegisterInfo.h"
#define GET_REGINFO_HEADER
#include "LVMGenRegisterInfo.inc"
namespace llvm {

    class LVMRegisterInfo : public LVMGenRegisterInfo {
    public:
        explicit LVMRegisterInfo(unsigned HwMode);
        const MCPhysReg *getCalleeSavedRegs(const MachineFunction *MF) const override;
        const uint32_t *getCallPreservedMask(const MachineFunction &MF, CallingConv::ID) const override;
        BitVector getReservedRegs(const MachineFunction &MF) const override;
        void eliminateFrameIndex(MachineBasicBlock::iterator MI, int SPAdj, unsigned FIOperandNum, RegScavenger *RS = nullptr) const override;
        Register getFrameRegister(const MachineFunction &MF) const override;
    };
 } // end namespace llvm
#endif