#ifndef LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMELFSTREAMER_H
#define LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMELFSTREAMER_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/MC/MCELFStreamer.h"

#include <memory>

namespace llvm {

class MCAsmBackend;
class MCCodeEmitter;
class MCContext;
class MCSubtargetInfo;

class LVMELFStreamer : public MCELFStreamer {

public:
  LVMELFStreamer(MCContext &Context, std::unique_ptr<MCAsmBackend> MAB,
                  std::unique_ptr<MCObjectWriter> OW,
                  std::unique_ptr<MCCodeEmitter> Emitter);
};

MCStreamer *createLVMELFStreamer(const Triple& trip, MCContext &Context,
                                     std::unique_ptr<MCAsmBackend>&& MAB,
                                     std::unique_ptr<MCObjectWriter>&& OW,
                                     std::unique_ptr<MCCodeEmitter>&& Emitter,
                                     bool RelaxAll);
} // namespace llvm

#endif
