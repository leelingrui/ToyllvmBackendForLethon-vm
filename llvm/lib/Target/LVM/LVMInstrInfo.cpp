#include "LVMInstrInfo.h"
#include "LVMSubtarget.h"
#include "LVMTargetMachine.h"
#include "LVMMachineFunctionInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/MC/TargetRegistry.h"

#define GET_INSTRINFO_CTOR_DTOR
#include "LVMGenInstrInfo.inc"
#define DEBUG_TYPE "LVM-instr-info"
using namespace llvm;

// Pin the vtable to this file
void LVMInstrInfo::anchor() { }

void llvm::LVMInstrInfo::storeRegToStack(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MI, unsigned SrcReg,
    bool isKill, int FrameIndex, const TargetRegisterClass *RC,
    const TargetRegisterInfo *TRI, int64_t Offset) const 
    {
        DebugLoc DL;
        MachineMemOperand *MMO = GetMemOperand(MBB, FrameIndex, MachineMemOperand::MOStore);
        unsigned Opc = 0;
        switch (RC->getID())
        {
        case LVM::GR8RegClassID:
            Opc = LVM::STR_8IR;
            break;
        case LVM::GR16RegClassID:
            Opc = LVM::STR_16IR;
            break;
        case LVM::GR32RegClassID:
            Opc = LVM::STR_32IR;
            break;
        case LVM::GR64RegClassID:
        case LVM::GR64_with_sub_8bitRegClassID:
            Opc = LVM::STR_64IR;
            break;
        default:
            llvm_unreachable("unknow register class");
            break;
        }
        assert(Opc && "Register class not handled!");
        BuildMI(MBB, MI, DL, get(Opc)).addFrameIndex(FrameIndex).addImm(Offset).addMemOperand(MMO).addReg(SrcReg, getKillRegState(isKill));
    }

    void llvm::LVMInstrInfo::loadRegFromStack(MachineBasicBlock &MBB,
                                              MachineBasicBlock::iterator MI,
                                              unsigned DestReg, int FrameIndex,
                                              const TargetRegisterClass *RC,
                                              const TargetRegisterInfo *TRI,
                                              int64_t Offset) const 
    {
        DebugLoc DL;
        if (MI != MBB.end()) DL = MI->getDebugLoc();
        MachineMemOperand *MMO = GetMemOperand(MBB, FrameIndex, MachineMemOperand::MOLoad);
        unsigned Opc = 0;

        switch (RC->getID())
        {
        case LVM::GR8RegClassID:
            Opc = LVM::LDR_8M;
            break;
        case LVM::GR16RegClassID:
            Opc = LVM::LDR_16M;
            break;
        case LVM::GR32RegClassID:
            Opc = LVM::LDR_32M;
            break;
        case LVM::GR64RegClassID:
        case LVM::GR64_with_sub_8bitRegClassID:
            Opc = LVM::LDR_64M;
            break;
        default:
            llvm_unreachable("unknow register class");
            break;
        }
        assert(Opc && "Register class not handled!");
        BuildMI(MBB, MI, DL, get(Opc), DestReg).addFrameIndex(FrameIndex).addImm(Offset).addMemOperand(MMO);
    }

    LVMInstrInfo::LVMInstrInfo(const LVMSubtarget &_STI)
        : LVMGenInstrInfo(LVM::ADJCALLSTACKUP, LVM::ADJCALLSTACKDOWN), STI(_STI), RI(0) {}

    void
    llvm::LVMInstrInfo::adjustStackPtr(unsigned SP, int64_t Amount,
                                       MachineBasicBlock &MBB,
                                       MachineBasicBlock::iterator I) const {
        DebugLoc DL = I != MBB.end() ? I->getDebugLoc() : DebugLoc();

        if (isInt<8>(Amount)) {
            BuildMI(MBB, I, DL, get(LVM::LDR_64I), LVM::RAX).addImm(Amount);
        } else { // Expand immediate that doesn't fit in 8-bit.
            unsigned target_reg = LVM::RAX;
            loadImmediate(Amount, target_reg, MBB, I, DL, nullptr);
        }
        BuildMI(MBB, I, DL, get(LVM::SUB_64rr), SP).addReg(SP).addReg(LVM::RAX);

}

void llvm::LVMInstrInfo::copyPhysReg(MachineBasicBlock &MBB, MachineBasicBlock::iterator MI, const DebugLoc &DL, MCRegister DestReg, MCRegister SrcReg, bool KillSrc) const
{
    unsigned Opc = 0;
    if (LVM::GR8RegClass.contains(DestReg, SrcReg))
    {
        Opc = LVM::MOV_8rr;
    }
    else if(LVM::GR16RegClass.contains(DestReg, SrcReg))
    {
        Opc = LVM::MOV_16rr;
    }
    else if(LVM::GR32RegClass.contains(DestReg, SrcReg))
    {
        Opc = LVM::MOV_32rr;
    }
    else if(LVM::GR64RegClass.contains(DestReg, SrcReg))
    {
        Opc = LVM::MOV_64rr;
    }
    if (Opc)
    {
        BuildMI(MBB, MI, DL, get(Opc), DestReg).addReg(SrcReg, getKillRegState(KillSrc));
        return;
    }
    LLVM_DEBUG(dbgs() << "Cannot copy " << RI.getName(SrcReg) << " to " << RI.getName(DestReg) << '\n');
    report_fatal_error("Cannot emit physreg copy instruction");
}

// const LVMInstrInfo *LVMInstrInfo::create(LVMSubtarget &STI) {
//     return llvm::createLVMSEInstrInfo(STI);
// }

void llvm::LVMInstrInfo::loadImmediate(int64_t Imm, MCPhysReg targetReg, MachineBasicBlock &MBB,
                                           MachineBasicBlock::iterator II,
                                           const DebugLoc &DL,
                                           unsigned *NewImm) const
{
    LVMAnalyzeImmediate AnalyzeImm;
    unsigned Size = 32;
    bool LastInstrIsADD = NewImm;
    const LVMAnalyzeImmediate::InstSeq &Seq = AnalyzeImm.Analyze(Imm, Size, LastInstrIsADD);
    LVMAnalyzeImmediate::InstSeq::const_iterator Inst = Seq.begin();
    assert(Seq.size() && (!LastInstrIsADD || (Seq.size() > 1)));
    for (; Inst != Seq.end() - LastInstrIsADD; ++Inst)
    {
        if (Inst->Opc == LVM::LDR_8I)
            BuildMI(MBB, II, DL, get(LVM::LDR_8I), targetReg).addImm(SignExtend64<8>(Inst->ImmOperand));
        else
            BuildMI(MBB, II, DL, get(Inst->Opc), targetReg).addImm(SignExtend64<8>(Inst->ImmOperand));
    }


}

unsigned LVMInstrInfo::GetInstSizeInBytes(const MachineInstr &MI) const {
    switch (MI.getOpcode()) {
        case TargetOpcode::INLINEASM: {
            const MachineFunction *MF = MI.getParent()->getParent();
            const char *AsmStr = MI.getOperand(0).getSymbolName();
            return getInlineAsmLength(AsmStr, *MF->getTarget().getMCAsmInfo());
        }
        default:
          return MI.getDesc().getSize();
    }
}

MachineMemOperand *LVMInstrInfo::GetMemOperand(MachineBasicBlock &MBB, int FI,
                             MachineMemOperand::Flags Flags) const {
    MachineFunction &MF = *MBB.getParent();
    MachineFrameInfo &MFI = MF.getFrameInfo();
    Align align = MFI.getObjectAlign(FI);

    return MF.getMachineMemOperand(MachinePointerInfo::getFixedStack(MF, FI),
                                  Flags, MFI.getObjectSize(FI), align);
}