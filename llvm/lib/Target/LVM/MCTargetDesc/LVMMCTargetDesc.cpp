#include "LVMMCTargetDesc.h"
#include "MCTargetDesc/LVMInstPrinter.h"
#include "LVMMCAsmInfo.h"
#include "TargetInfo/LVMTargetInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "LVMELFStreamer.h"

#define GET_INSTRINFO_MC_DESC
#include "LVMGenInstrInfo.inc"

#define GET_REGINFO_MC_DESC
#include "LVMGenRegisterInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "LVMGenSubtargetInfo.inc"

using namespace llvm;

namespace {

MCInstrInfo *createLVMMCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitLVMMCInstrInfo(X);
  return X;
}

MCRegisterInfo *createLVMMCRegisterInfo(const Triple &TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitLVMMCRegisterInfo(X, LVM::RAX);
  return X;
}

MCAsmInfo *createLVMMCAsmInfo(const MCRegisterInfo &MRI, const Triple &TT,
                                  const MCTargetOptions &Options) {
  return new LVMMCAsmInfo(TT);
}

MCSubtargetInfo *createLVMMCSubtargetInfo(const Triple &TT, StringRef CPU,
                                              StringRef FS) {
    if (CPU.empty()) {
            assert(TT.isArch64Bit() && "Only LVM64 is currently supported!");
            CPU = "lethon-64";
    }
    return createLVMMCSubtargetInfoImpl(TT, CPU, CPU, FS);
}

MCInstPrinter *createLVMMCInstPrinter(const Triple &T,
                                          unsigned SyntaxVariant,
                                          const MCAsmInfo &MAI,
                                          const MCInstrInfo &MII,
                                          const MCRegisterInfo &MRI) {
    return new LVMInstPrinter(MAI, MII, MRI);
}

} // end anonymous namespace

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeLVMTargetMC() {
    Target &LVM64 = getLVMTarget();
    TargetRegistry::RegisterMCAsmInfo(LVM64, createLVMMCAsmInfo);
    TargetRegistry::RegisterMCInstrInfo(LVM64, createLVMMCInstrInfo);
    TargetRegistry::RegisterMCRegInfo(LVM64, createLVMMCRegisterInfo);
    TargetRegistry::RegisterMCSubtargetInfo(LVM64, createLVMMCSubtargetInfo);
    TargetRegistry::RegisterMCInstPrinter(LVM64, createLVMMCInstPrinter);
    TargetRegistry::RegisterMCCodeEmitter(LVM64, createLVMMCCodeEmitter);
    TargetRegistry::RegisterELFStreamer(LVM64, createLVMELFStreamer);
    TargetRegistry::RegisterMCAsmBackend(LVM64, createLVMAsmBackend);

}