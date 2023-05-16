#ifndef LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMINSTPRINTER_H
#define LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMINSTPRINTER_H

#include "llvm/MC/MCInstPrinter.h"

namespace llvm {

class LVMInstPrinter : public MCInstPrinter {
public:
    LVMInstPrinter(const MCAsmInfo &MAI, const MCInstrInfo &MII, const MCRegisterInfo &MRI)  : MCInstPrinter(MAI, MII, MRI) {};

    void printInst(const MCInst *MI, uint64_t Address, StringRef Annot,
                    const MCSubtargetInfo &STI, raw_ostream &OS) override;
private:
    std::pair<const char *, uint64_t> getMnemonic(const MCInst *MI) override;

    void printInstruction(const MCInst *MI, uint64_t Address, raw_ostream &O);

    bool printAliasInstr(const MCInst *MI, uint64_t Address, raw_ostream &O);

    static const char *getRegisterName(unsigned RegNo);

    void printCustomAliasOperand(const MCInst *MI, uint64_t Address,
                                unsigned OpIdx, unsigned PrintMethodIdx,
                                raw_ostream &O);
    void printMemOperand(const MCInst *MI, int OpNum, raw_ostream &O);
    void printOperand(const MCInst *MI, unsigned OpNo, raw_ostream &O);
    void printOperand(const MCInst *MI, uint64_t Address, unsigned OpNo, raw_ostream &O);

};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMINSTPRINTER_H