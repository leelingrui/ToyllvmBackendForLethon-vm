#include "LVMELFStreamer.h"
#include "LVMTargetStreamer.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/Support/Casting.h"

using namespace llvm;

LVMELFStreamer::LVMELFStreamer(MCContext &Context,
                                 std::unique_ptr<MCAsmBackend> MAB,
                                 std::unique_ptr<MCObjectWriter> OW,
                                 std::unique_ptr<MCCodeEmitter> Emitter)
    : MCELFStreamer(Context, std::move(MAB), std::move(OW),
                    std::move(Emitter)) {}

MCStreamer *llvm::createLVMELFStreamer(const Triple& trip, MCContext &Context,
                                           std::unique_ptr<MCAsmBackend>&& MAB,
                                           std::unique_ptr<MCObjectWriter>&& OW,
                                           std::unique_ptr<MCCodeEmitter>&& Emitter,
                                           bool RelaxAll) {
  return new LVMELFStreamer(Context, std::move(MAB), std::move(OW),
                             std::move(Emitter));
}