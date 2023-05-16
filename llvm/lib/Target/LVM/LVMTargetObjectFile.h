#ifndef LLVM_LIB_TARGET_LVM_LVMTARGETOBJECTFILE_H
#define LLVM_LIB_TARGET_LVM_LVMTARGETOBJECTFILE_H

#include "LVMTargetMachine.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"

namespace llvm {
class LVMTargetMachine;
class LVMTargetObjectFile : public TargetLoweringObjectFileELF {
  MCSection *SmallDataSection;
  MCSection *SmallBSSSection;
  const LVMTargetMachine *TM;

  bool IsGlobalInSmallSection(const GlobalObject *GO,
                              const TargetMachine &TM, SectionKind Kind) const;
  bool IsGlobalInSmallSectionImpl(const GlobalObject *GO,
                                  const TargetMachine &TM) const;
public:
  void Initialize(MCContext &Ctx, const TargetMachine &TM) override;

  // Return true if this global address should be
  // placed into small data/bss section.
  bool IsGlobalInSmallSection(const GlobalObject *GO,
                              const TargetMachine &TM) const;

  MCSection *SelectSectionForGlobal(const GlobalObject *GO, SectionKind Kind,
                                    const TargetMachine &TM) const override;

};
} // end namespace llvm

#endif
