#include "MCTargetDesc/LVMMCTargetDesc.h"
#include "LVMMCInstLower.h"
#include "TargetInfo/LVMTargetInfo.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/TargetRegistry.h"

using namespace llvm;

namespace {

class LVMAsmPrinter : public AsmPrinter {
    LVMMCInstLower MCInstLowering;

public:
    LVMAsmPrinter(TargetMachine &TM, std::unique_ptr<MCStreamer> Streamer)
        : AsmPrinter(TM, std::move(Streamer)), MCInstLowering(OutContext, *this) {
    }

    void emitInstruction(const MachineInstr *MI) override;

private:
    bool emitPseudoExpansionLowering(MCStreamer &OutStreamer,
                                    const MachineInstr *MI);
};

} // end anonymous namespace

#include "LVMGenMCPseudoLowering.inc"

void LVMAsmPrinter::emitInstruction(const MachineInstr *MI) {
    if (emitPseudoExpansionLowering(*OutStreamer, MI)) {
        return;
    }

    MCInst TmpInst;
    MCInstLowering.Lower(MI, TmpInst);
    EmitToStreamer(*OutStreamer, TmpInst);
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeLVMAsmPrinter() {
    RegisterAsmPrinter<LVMAsmPrinter> X(getLVMTarget());
}