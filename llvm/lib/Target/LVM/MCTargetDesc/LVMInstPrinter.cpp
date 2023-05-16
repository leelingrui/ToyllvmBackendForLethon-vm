#include "LVMInstPrinter.h"
#include "MCTargetDesc/LVMMCTargetDesc.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

#define PRINT_ALIAS_INSTR
#include "LVMGenAsmWriter.inc"
#define DEBUG_TYPE "LVM-asm-printer"

void LVMInstPrinter::printInst(const MCInst *MI, uint64_t Address,
                               StringRef Annot, const MCSubtargetInfo &STI,
                               raw_ostream &OS) {
  if (!printAliasInstr(MI, Address, OS)) {
    printInstruction(MI, Address, OS);
  }
}

void llvm::LVMInstPrinter::printMemOperand(const MCInst *MI, int OpNum,
                                           raw_ostream &O)
{
    printOperand(MI, OpNum+1, O);
    O << "(";
    printOperand(MI, OpNum, O);
    O << ")";
}

void LVMInstPrinter::printOperand(const MCInst *MI, unsigned OpNo, raw_ostream &OS) {
    const MCOperand &MO = MI->getOperand(OpNo);
    if (MO.isReg())
    {
        OS << getRegisterName(MO.getReg());
        return;
    }
    if (MO.isImm())
    {
        OS << MO.getImm();
        return;
    }
    assert(MO.isExpr() && "Unknown operand kind in printOperand!");
    MO.getExpr()->print(OS, &MAI);
}

void llvm::LVMInstPrinter::printOperand(const MCInst *MI, uint64_t Address, unsigned OpNo, raw_ostream &OS)
{
    const MCOperand &MO = MI->getOperand(OpNo);

    if (MO.isReg())
    {
        OS << getRegisterName(MO.getReg());
        return;
    }
    if (MO.isImm())
    {
        OS << MO.getImm();
        return;
    }
    assert(MO.isExpr() && "Unknown operand kind in printOperand!");
    MO.getExpr()->print(OS, &MAI);
}
