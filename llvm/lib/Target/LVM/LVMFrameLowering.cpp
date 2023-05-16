#include "LVMFrameLowering.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "LVMSubtarget.h"

using namespace llvm;

void LVMFrameLowering::emitPrologue(MachineFunction &MF, MachineBasicBlock &MBB) const
{
    assert(&MF.front() == &MBB && "Shrink-wrapping not yet supported");
    MachineFrameInfo &MFI    = MF.getFrameInfo();
    LVMMachineFunctionInfo *LVMFI = MF.getInfo<LVMMachineFunctionInfo>();

    const LVMInstrInfo &TII = *static_cast<const LVMInstrInfo*>(STI.getInstrInfo());
    const LVMRegisterInfo &RegInfo = *static_cast<const LVMRegisterInfo*>(STI.getRegisterInfo());

    MachineBasicBlock::iterator MBBI = MBB.begin();
    DebugLoc dl = MBBI != MBB.end() ? MBBI->getDebugLoc() : DebugLoc();
    LVMABIInfo ABI = STI.getABI();
    unsigned SP = LVM::RSP;
    const TargetRegisterClass *RC = &LVM::GR64RegClass;

    // First, compute final stack size.
    uint64_t StackSize = MFI.getStackSize();

    // No need to allocate space on the stack.
    if (StackSize == 0 && !MFI.adjustsStack()) return;

    MachineModuleInfo &MMI = MF.getMMI();
    const MCRegisterInfo *MRI = MMI.getContext().getRegisterInfo();

    // Adjust stack.
    
    if (StackSize)
    {
        BuildMI(MBB, MBBI, dl, TII.get(LVM::PUSH_64R), LVM::RBP);
        BuildMI(MBB, MBBI, dl, TII.get(LVM::MOV_64rr), LVM::RBP).addReg(LVM::RSP);
        TII.adjustStackPtr(SP, StackSize, MBB, MBBI);
    }


    // emit ".cfi_def_cfa_offset StackSize"
    unsigned CFIIndex = MF.addFrameInst(
        MCCFIInstruction::cfiDefCfaOffset(nullptr, StackSize));
    BuildMI(MBB, MBBI, dl, TII.get(TargetOpcode::CFI_INSTRUCTION))
        .addCFIIndex(CFIIndex);

    const std::vector<CalleeSavedInfo> &CSI = MFI.getCalleeSavedInfo();
    if (CSI.size()) {
        // Find the instruction past the last instruction that saves a callee-saved
        // register to the stack.
        for (unsigned i = 0; i < CSI.size(); ++i)
        ++MBBI;

        // Iterate over list of callee-saved registers and emit .cfi_offset
        // directives.
        for (std::vector<CalleeSavedInfo>::const_iterator I = CSI.begin(),
            E = CSI.end(); I != E; ++I) {
        int64_t Offset = MFI.getObjectOffset(I->getFrameIdx());
        unsigned Reg = I->getReg();
        {
            // Reg is in CPURegs
            unsigned CFIIndex = MF.addFrameInst(MCCFIInstruction::createOffset(
                nullptr, MRI->getDwarfRegNum(Reg, 1), Offset));
            BuildMI(MBB, MBBI, dl, TII.get(TargetOpcode::CFI_INSTRUCTION))
                .addCFIIndex(CFIIndex);
        }
        }
    }
}

void LVMFrameLowering::emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const
{
    MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
    MachineFrameInfo &MFI = MF.getFrameInfo();
    LVMMachineFunctionInfo *LVMFI = MF.getInfo<LVMMachineFunctionInfo>();

    const LVMInstrInfo &TII =
        *static_cast<const LVMInstrInfo*>(STI.getInstrInfo());
    const LVMRegisterInfo &RegInfo =
        *static_cast<const LVMRegisterInfo*>(STI.getRegisterInfo());

    DebugLoc dl = MBBI->getDebugLoc();
    LVMABIInfo ABI = STI.getABI();
    // Get the number of bytes from FrameInfo
    uint64_t StackSize = MFI.getStackSize();
    // Adjust stack.
    if (StackSize)
    {
        BuildMI(MBB, MBBI, dl, TII.get(LVM::MOV_64rr), LVM::RSP).addReg(LVM::RBP);
        BuildMI(MBB, MBBI, dl, TII.get(LVM::POP_64R), LVM::RBP);
    }

}
bool LVMFrameLowering::hasFP(const MachineFunction &MF) const
{
    return true;
}

MachineBasicBlock::iterator llvm::LVMFrameLowering::eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
    MachineBasicBlock::iterator I) const
{
    return MBB.erase(I);
}
