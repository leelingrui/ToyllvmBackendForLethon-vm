#include "LVMTargetStreamer.h"
#include "MCTargetDesc/LVMInstPrinter.h"
#include "LVMELFStreamer.h"
#include "LVMMCTargetDesc.h"
#include "LVMTargetObjectFile.h"
#include "LVMTargetStreamer.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

LVMTargetStreamer::LVMTargetStreamer(MCStreamer &S)
    : MCTargetStreamer(S) {}

LVMTargetAsmStreamer::LVMTargetAsmStreamer(MCStreamer &S,
                                             formatted_raw_ostream &OS)
    : LVMTargetStreamer(S), OS(OS) {}