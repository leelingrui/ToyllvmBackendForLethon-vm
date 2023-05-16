#include "LVMMCInstLower.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
using namespace llvm;
LVMMCInstLower::LVMMCInstLower(MCContext &Ctx, AsmPrinter &Printer): Ctx(Ctx), Printer(Printer) {}
void LVMMCInstLower::Lower(const MachineInstr *MI, MCInst &OutMI) const 
{
    OutMI.setOpcode(MI->getOpcode());
    for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
        const MachineOperand &MO = MI->getOperand(i);
        MCOperand MCOp = LowerOperand(MO);

        if (MCOp.isValid())
            OutMI.addOperand(MCOp);
    }
}
MCOperand LVMMCInstLower::LowerOperand(const MachineOperand &MO, unsigned offset) const
{
    MachineOperandType MOTy = MO.getType();

    switch (MOTy) {
    default: llvm_unreachable("unknown operand type");
    case MachineOperand::MO_Register:
        // Ignore all implicit register operands
        if (MO.isImplicit()) break;
            return MCOperand::createReg(MO.getReg());
    case MachineOperand::MO_Immediate:
        return MCOperand::createImm(MO.getImm() + offset);
    case MachineOperand::MO_MachineBasicBlock:
    case MachineOperand::MO_JumpTableIndex:
    case MachineOperand::MO_BlockAddress:
    case MachineOperand::MO_GlobalAddress:
    case MachineOperand::MO_ExternalSymbol:
        return LowerSymbolOperand(MO, MOTy, offset);
    case MachineOperand::MO_RegisterMask:
        break;
    }

    return MCOperand();
}

MCOperand LVMMCInstLower::LowerSymbolOperand(const MachineOperand &MO, MachineOperandType MOTy, unsigned Offset) const {
    MCSymbolRefExpr::VariantKind Kind = MCSymbolRefExpr::VK_None;
    LVMMCExpr::LVMExprKind TargetKind = LVMMCExpr::LVMExprKind::CEK_None;
    const MCSymbol *Symbol;

    switch(MO.getTargetFlags()) {
    default:
        llvm_unreachable("Invalid target flag!");
    case LVMII::MO_NO_FLAG:
        break;
    case LVMII::MO_GPREL:
        TargetKind = LVMMCExpr::CEK_GPREL;
        break;
    case LVMII::MO_GOT:
        TargetKind = LVMMCExpr::CEK_GOT;
        break;
    case LVMII::MO_ABS_HI:
        TargetKind = LVMMCExpr::CEK_ABS_HI;
        break;
    case LVMII::MO_ABS_LO:
        TargetKind = LVMMCExpr::CEK_ABS_LO;
        break;
    case LVMII::MO_GOT_HI16:
        TargetKind = LVMMCExpr::CEK_GOT_HI16;
        break;
    case LVMII::MO_GOT_LO16:
        TargetKind = LVMMCExpr::CEK_GOT_LO16;
        break;
    case LVMII::MO_GOT_CALL:
        TargetKind = LVMMCExpr::CEK_GOT_CALL;
        break;
    }

    switch (MOTy) {
    case MachineOperand::MO_GlobalAddress:
        Symbol = Printer.getSymbol(MO.getGlobal());
        Offset += MO.getOffset();
        break;
    case MachineOperand::MO_MachineBasicBlock:
        Symbol = MO.getMBB()->getSymbol();
        break;
    case MachineOperand::MO_BlockAddress:
        Symbol = Printer.GetBlockAddressSymbol(MO.getBlockAddress());
        Offset += MO.getOffset();
        break;
    case MachineOperand::MO_JumpTableIndex:
        Symbol = Printer.GetJTISymbol(MO.getIndex());
        break;
    case MachineOperand::MO_ExternalSymbol:
        Symbol = Printer.GetExternalSymbolSymbol(MO.getSymbolName());
        Offset += MO.getOffset();
        break;
    default:
        llvm_unreachable("unknown operand type");
    }

    const MCExpr *Expr = MCSymbolRefExpr::create(Symbol, Kind, Ctx);

    if (Offset) {
        // Assume offset is never negative.
        assert(Offset > 0);
        Expr = MCBinaryExpr::createAdd(Expr, MCConstantExpr::create(Offset, Ctx), Ctx);
    }

    if (TargetKind != LVMMCExpr::CEK_None)
        Expr = LVMMCExpr::create(TargetKind, Expr, Ctx);

    return MCOperand::createExpr(Expr);
}