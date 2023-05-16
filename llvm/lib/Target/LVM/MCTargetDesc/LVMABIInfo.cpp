#include "LVMABIInfo.h"
#include "LVMInstrInfo.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/Support/CommandLine.h"
#include "LVMRegisterInfo.h"
using namespace llvm;

static cl::opt<bool>
EnableLVMCalls("lvm-calls", cl::Hidden,
                   cl::desc("LVM call: use two register and stack pass arguments."),
                   cl::init(false));

namespace {
  static const MCPhysReg IntRegs[2] = {LVM::RCX, LVM::RDX};
}

const ArrayRef<MCPhysReg> LVMABIInfo::GetByValArgRegs() const {
  return IntRegs;
}

const ArrayRef<MCPhysReg> LVMABIInfo::GetVarArgRegs() const {
  return IntRegs;
}

const MCPhysReg* LVMABIInfo::GetCalleeAllocdArgSizeInBytes(CallingConv::ID CC) const {
  return IntRegs;
}

LVMABIInfo LVMABIInfo::computeTargetABI() {
  return ABI::Default;
}

unsigned LVMABIInfo::GetStackPtr() const { return LVM::RSP; }

unsigned LVMABIInfo::GetFramePtr() const { return LVM::RBP; }

unsigned LVMABIInfo::GetEhDataReg(unsigned I) const {
  static const unsigned EhDataReg[] = { LVM::RCX, LVM::RDX };

  return EhDataReg[I];
}

int LVMABIInfo::EhDataRegSize() const {
    return 2;
}