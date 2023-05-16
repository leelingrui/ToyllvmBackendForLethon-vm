#ifndef LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMABIINFO_H
#define LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMABIINFO_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Triple.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "LVMMCTargetDesc.h"

namespace llvm {

class MCTargetOptions;
class StringRef;
class TargetRegisterClass;

class LVMABIInfo {
public:
  enum class ABI { Unknown, Default };

protected:
  ABI ThisABI;

public:

  LVMABIInfo(ABI ThisABI) : ThisABI(ThisABI) { }

  static LVMABIInfo Unknown() { return LVMABIInfo(ABI::Unknown); }
  static LVMABIInfo computeTargetABI();
  ABI GetEnumValue() const { return ThisABI; }

  // The registers to use for byval arguments
  const ArrayRef<MCPhysReg> GetByValArgRegs() const;

  // The registers to use for variable argument list
  const ArrayRef<MCPhysReg> GetVarArgRegs() const;

  // Obtain the size of the area allocated by the callee for arguments
  // CallingConv::FastCall affects the value for O32
  const MCPhysReg* GetCalleeAllocdArgSizeInBytes(CallingConv::ID CC) const;

  // LVMGenSubtargetInfo.inc will use this to resolve conflicts when given
  // multiple ABI options
  bool operator<(const LVMABIInfo Other) const {
    return ThisABI < Other.GetEnumValue();
  }

  unsigned GetStackPtr() const;
  unsigned GetFramePtr() const;

  unsigned GetEhDataReg(unsigned I) const;
  int EhDataRegSize() const;
};
} // End llvm namespace

#endif