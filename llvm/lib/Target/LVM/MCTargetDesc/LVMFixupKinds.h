#ifndef LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMFIXUPKINDS_H
#define LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMFIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

namespace llvm {
namespace LVM {
  // Although most of the current fixup types reflect a unique relocation
  // one can have multiple fixup types for a given relocation and thus need
  // to be uniquely named.

  // This table *must* be in the save order of
  // MCFixupKindInfo Infos[LVM::NumTargetFixupKinds]
  // in LVMAsmBackend.cpp.
  enum Fixups {
    // Pure upper 32 bit fixup resulting in - R_LVM_32.
    fixup_LVM_64 = FirstTargetFixupKind,

    fixup_LVM_GR64REL,

    fixup_LVM_PC8,

    // Marker
    LastTargetFixupKind,
    NumTargetFixupKinds = LastTargetFixupKind - FirstTargetFixupKind
  };
} // namespace LVM
} // namespace llvm

#endif
