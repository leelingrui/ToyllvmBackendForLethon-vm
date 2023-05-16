#include "LVMSubtarget.h"
#include "LVMTargetMachine.h"

using namespace llvm;

#define DEBUG_TYPE "LVM-subtarget"

// #define GET_SUBTARGETINFO_MC_DESC
#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "LVMGenSubtargetInfo.inc"

bool llvm::LVMSubtarget::abiUsesSoftFloat() const { return false; }

LVMSubtarget::LVMSubtarget(const Triple &TT, StringRef CPU, StringRef TuneCPU,
                           StringRef FS, LVMTargetMachine &TM)
    : LVMGenSubtargetInfo(TT, CPU, TuneCPU, FS), TLInfo(TM),
      FrameLowering(initializeSubtargetDependencies(TT, CPU, FS)),
      InstrInfo(*reinterpret_cast<const LVMSubtarget *>(this)),
      RegInfo(getHwMode()), ABI(LVMABIInfo::ABI::Default)
{
    IsLittle = true;
}

const LVMABIInfo &LVMSubtarget::getABI() const {
  return ABI;
}

LVMSubtarget & LVMSubtarget::initializeSubtargetDependencies(const Triple &TT,\
    StringRef CPU, StringRef FS) {
    if (CPU.empty()) 
    {
        assert(TT.isArch64Bit() && "Only LMV64 is currently supported!");
        CPU = "lvm-64";
    }
    ParseSubtargetFeatures(CPU, CPU, FS);
    return *this;
}