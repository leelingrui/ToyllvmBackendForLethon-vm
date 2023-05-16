#include "LVMRegisterInfo.h"
#include "MCTargetDesc/LVMMCTargetDesc.h"
#include "LVMFrameLowering.h"
#include "LVMSubtarget.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/Support/Alignment.h"
#define GET_REGINFO_TARGET_DESC
#include "LVMGenRegisterInfo.inc"
#define DEBUG_TYPE "LVM-reg-info"

namespace llvm
{
    LVMRegisterInfo::LVMRegisterInfo(unsigned HwMode) : LVMGenRegisterInfo(LVM::IP, /*DwarfFlavour*/ 0, /*EHFlavor*/ 0, /*PC*/ 0, HwMode) {}
    const MCPhysReg *LVMRegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
        return CC_Save_SaveList;
    }

    const uint32_t *
    LVMRegisterInfo::getCallPreservedMask(const MachineFunction &MF, CallingConv::ID) const {
        return CC_Save_RegMask;
    }

    BitVector LVMRegisterInfo::getReservedRegs(const MachineFunction &MF) const {
        BitVector Reserved(getNumRegs());
        Reserved.set(LVM::RBP);
        Reserved.set(LVM::RCS);
        Reserved.set(LVM::RDS);
        Reserved.set(LVM::RES);
        Reserved.set(LVM::RFS);
        Reserved.set(LVM::RGS);
        Reserved.set(LVM::RFLAGS);
        return Reserved;
    }
    void LVMRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator MI, int SPAdj, unsigned FIOperandNum, RegScavenger *RS) const
    {
        MachineInstr &MII = *MI;
        MachineFunction &MF = *MII.getParent()->getParent();
        MachineFrameInfo &MFI = MF.getFrameInfo();
        //Cpu0MachineFunctionInfo *Cpu0FI = MF.getInfo<Cpu0MachineFunctionInfo>();

        unsigned i = 0;
        while (!MII.getOperand(i).isFI()) {
            ++i;
            assert(i < MII.getNumOperands() && "Instr doesn't have FrameIndex operand!");
        }

        LLVM_DEBUG(errs() << "\nFunction : " << MF.getFunction().getName() << "\n"; errs() << "<---------->\n" << MII);
        int FrameIndex = MII.getOperand(i).getIndex();
        uint64_t stackSize = MFI.getStackSize();
        int64_t ObjSize = MFI.getObjectAlign(FrameIndex).value();
        int64_t bpOffset = MFI.getObjectOffset(FrameIndex) + ObjSize;

        LLVM_DEBUG(errs() << "FrameIndex : " << FrameIndex << "\n" << "bpOffset   : " << bpOffset << "\n" << "stackSize  : " << stackSize << "\n" << "ObjectSize : " << ObjSize << "\n");

        const std::vector<CalleeSavedInfo> &CSI = MFI.getCalleeSavedInfo();
        int MinCSFI = 0;
        int MaxCSFI = -1;

        if (CSI.size()) {
            MinCSFI = CSI[0].getFrameIdx();
            MaxCSFI = CSI[CSI.size() - 1].getFrameIdx();
        }

        // The following stack frame objects are always referenced relative to $sp:
        //  1. Outgoing arguments.
        //  2. Pointer to dynamically allocated stack space.
        //  3. Locations for callee-saved registers.
        // Everything else is referenced relative to whatever register
        // getFrameRegister() returns.
        unsigned FrameReg;

        FrameReg = getFrameRegister(MF);

        // Calculate final offset.
        // There is no need to change the offset if the frame object is one of the
        // following: an outgoing argument, pointer to a dynamically allocated
        // stack space or a $gp restore location,
        // If the frame object is any of the following, its offset must be adjust
        // by adding the size of the stack: incomming argument, callee-saved register
        // location or local variable.
        int64_t Offset;
        Offset = bpOffset + (int64_t)stackSize;

        Offset -= MII.getOperand(i+1).getImm();

        LLVM_DEBUG(errs() << "Offset : " << -Offset << "\n" << "<---------->\n");

        // If MI is not a debug value, make sure Offset fits in the 8-bit immediate
        // field.
        if (!MII.isDebugValue() && !isInt<8>(-Offset)) {
            assert(false && "(!MI.isDebugValue() && !isInt<8>(Offset))");
        }

        MII.getOperand(i).ChangeToRegister(FrameReg, false);
        MII.getOperand(i+1).ChangeToImmediate(-Offset);
        LLVM_DEBUG(errs() << "Create Machine Instruction:" << MII << "\n");
    }
    Register LVMRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
        return LVM::RBP;
    }
}