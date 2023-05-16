#include "LVMMachineFunctionInfo.h"

#include "LVMInstrInfo.h"
#include "LVMSubtarget.h"
#include "llvm/IR/Function.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "LVMRegisterInfo.h"
// #define GET_REGINFO_ENUM
// #include "LVMGenRegisterInfo.inc"

using namespace llvm;

bool FixGlobalBaseReg;

LVMMachineFunctionInfo::~LVMMachineFunctionInfo() { }

bool LVMMachineFunctionInfo::globalBaseRegFixed() const {
  return FixGlobalBaseReg;
}

bool LVMMachineFunctionInfo::globalBaseRegSet() const {
  return GlobalBaseReg;
}

unsigned LVMMachineFunctionInfo::getGlobalBaseReg() {
  return GlobalBaseReg = LVM::RBP;
}

void LVMMachineFunctionInfo::anchor() { }

MachinePointerInfo LVMMachineFunctionInfo::callPtrInfo(const char *ES) {
  return MachinePointerInfo(MF.getPSVManager().getExternalSymbolCallEntry(ES));
}

MachinePointerInfo LVMMachineFunctionInfo::callPtrInfo(const GlobalValue *GV) {
  return MachinePointerInfo(MF.getPSVManager().getGlobalValueCallEntry(GV));
}
