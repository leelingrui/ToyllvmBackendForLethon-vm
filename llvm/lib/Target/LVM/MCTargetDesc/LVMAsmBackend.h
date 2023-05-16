#ifndef LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMASMBACKEND_H
#define LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMASMBACKEND_H

#include "MCTargetDesc/LVMFixupKinds.h"
#include "llvm/ADT/Triple.h"
#include "llvm/MC/MCAsmBackend.h"
#include "LVMELFObjectWriter.h"
namespace llvm {
  class MCAssembler;
  struct MCFixupKindInfo;
  class MCObjectWriter;
  class MCRegisterInfo;
  class MCSymbolELF;
  class Target;

  class LVMAsmBackend : public MCAsmBackend {
        Triple TheTriple;

  public:
        LVMAsmBackend(const Target &T, const MCRegisterInfo &MRI, const Triple &TT,
                    StringRef CPU)
            : MCAsmBackend(TT.isLittleEndian() ? support::little : support::big),
            TheTriple(TT) {}

        std::unique_ptr<MCObjectTargetWriter>
        createObjectTargetWriter() const override;

        void applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                        const MCValue &Target, MutableArrayRef<char> Data,
                        uint64_t Value, bool IsResolved,
                        const MCSubtargetInfo *STI) const override;

        const MCFixupKindInfo &getFixupKindInfo(MCFixupKind Kind) const override;

        unsigned getNumFixupKinds() const override {
        return LVM::NumTargetFixupKinds;
        }

        /// Target Relaxation Interfaces
        /// @{

        /// Check whether the given instruction may need relaxation
        bool mayNeedRelaxation(const MCInst &Inst,
                            const MCSubtargetInfo &STI) const override {
        return false;
        }

        /// Target specific predicate for whether a given fixup registers
        /// the associated instruction to be relaxed.
        bool fixupNeedsRelaxation(const MCFixup &Fixup, uint64_t Value,
                                const MCRelaxableFragment *DF,
                                const MCAsmLayout &Layout) const override {
        /// FIXME.
            llvm_unreachable("RelaxInstruction() unimplemented");
            return false;
        }

        /// Relax the instruction in the given fragment to the next wider instruction.
        void relaxInstruction(MCInst &Inst, const MCSubtargetInfo &STI) const override {}

        /// @}

        bool writeNopData(raw_ostream &OS, uint64_t Count, const MCSubtargetInfo *STI) const override;
  };  // class LVMAsmBackend

}  // namespace

#endif