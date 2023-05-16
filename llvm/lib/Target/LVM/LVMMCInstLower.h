#ifndef LLVM_LIB_TARGET_LVM_LVMMCINSTLOWER_H
#define LLVM_LIB_TARGET_LVM_LVMMCINSTLOWER_H
#include "MCTargetDesc/LVMMCExpr.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/Support/Compiler.h"
#include "MCTargetDesc/LVMBaseInfo.h"
namespace llvm {
    class MCContext;
    class AsmPrinter;
    class MachineInstr;
    class MachineOperand;
    class MCInst;
    class MCOperand;
    class LVMMCInstLower 
    {
        typedef MachineOperand::MachineOperandType MachineOperandType;
        MCContext &Ctx;
        AsmPrinter &Printer;

    public:
        LVMMCInstLower(MCContext &Ctx, AsmPrinter &Printer);
        void Lower(const MachineInstr *MI, MCInst &OutMI) const;
    private:
        MCOperand LowerOperand(const MachineOperand &MO, unsigned offset = 0) const;
        MCOperand LowerSymbolOperand(const MachineOperand &MO, MachineOperandType MOTy, unsigned Offset) const;
    };
} // end namespace llvm
#endif