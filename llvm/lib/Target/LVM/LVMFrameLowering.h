#ifndef LLVM_LIB_TARGET_LVM_LVMFRAMELOWERING_H
#define LLVM_LIB_TARGET_LVM_LVMFRAMELOWERING_H
 
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "LVMMachineFunctionInfo.h"
#include "LVMRegisterInfo.h"
#include "LVMInstrInfo.h"
#include "MCTargetDesc/LVMMCTargetDesc.h"
#include "MCTargetDesc/LVMABIInfo.h"
#include "llvm/CodeGen/MachineModuleInfo.h"

namespace llvm {
 
    class LVMSubtarget;
 
    class LVMFrameLowering : public TargetFrameLowering {
    const LVMSubtarget &STI;

    public:
        explicit LVMFrameLowering(const LVMSubtarget &STI) : TargetFrameLowering(StackGrowsDown,
        /*StackAlignment=*/Align(16),
        /*LocalAreaOffset=*/0),
        STI(STI) {}

        void emitPrologue(MachineFunction &MF, MachineBasicBlock &MBB) const override;

        void emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const override;

        bool hasFP(const MachineFunction &MF) const override;
        MachineBasicBlock::iterator eliminateCallFramePseudoInstr(MachineFunction &MF,
                                MachineBasicBlock &MBB, MachineBasicBlock::iterator I) const override;
    };

} // end namespace llvm

#endif // LLVM_LIB_TARGET_LVM_LVMFRAMELOWERING_H
