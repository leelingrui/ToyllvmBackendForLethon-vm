#include "TargetInfo/LVMTargetInfo.h"
#include "llvm/MC/TargetRegistry.h"

using namespace llvm;

namespace llvm {

Target &getLVMTarget() {
  static Target LVMTarget;
  return LVMTarget;
}

} // end namespace llvm

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeLVMTargetInfo() {
  RegisterTarget<Triple::lvm> X(getLVMTarget(), "lethon vm",
                                      "lethon virtual machine", "self define instructions");
}