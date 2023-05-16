#ifndef LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMMCTARGETDESC_H
#define LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMMCTARGETDESC_H

#include "llvm/Support/DataTypes.h"

#include <memory>

namespace llvm {
  class MCAsmBackend;
  class MCCodeEmitter;
  class MCContext;
  class MCInstrInfo;
  class MCObjectTargetWriter;
  class MCRegisterInfo;
  class MCSubtargetInfo;
  class MCTargetOptions;
  class StringRef;
  class Target;
  class Triple;
  class raw_ostream;
  class raw_pwrite_stream;

  MCCodeEmitter *createLVMMCCodeEmitter(const MCInstrInfo &MCII,
                                           MCContext &Ctx);

  MCAsmBackend *createLVMAsmBackend(const Target &T,
                                         const MCSubtargetInfo &STI,
                                         const MCRegisterInfo &MRI,
                                         const MCTargetOptions &Options);

  std::unique_ptr<MCObjectTargetWriter>
  createLVMELFObjectWriter(const Triple &TT);
}

// Defines symbolic names for LVM registers. This defines a mapping from
//  register name to register number.
#define GET_REGINFO_ENUM
#include "LVMGenRegisterInfo.inc"

// Defines symbolic names for LVM instructions.
#define GET_INSTRINFO_ENUM
#include "LVMGenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "LVMGenSubtargetInfo.inc"

#endif
