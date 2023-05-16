#ifndef LLVM_LIB_TARGET_LVM_LVMTARGETSTREAMER_H
#define LLVM_LIB_TARGET_LVM_LVMTARGETSTREAMER_H

#include "llvm/MC/MCELFStreamer.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Support/FormattedStream.h"

namespace llvm {
class LVMTargetStreamer : public MCTargetStreamer {
public:
  LVMTargetStreamer(MCStreamer &S);
};

// This part is for ascii assembly output
class LVMTargetAsmStreamer : public LVMTargetStreamer {
  formatted_raw_ostream &OS;

public:
  LVMTargetAsmStreamer(MCStreamer &S, formatted_raw_ostream &OS);
};

}

#endif
