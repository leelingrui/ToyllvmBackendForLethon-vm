#ifndef LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMELFOBJECTWRITER_H
#define LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMELFOBJECTWRITER_H
#include "MCTargetDesc/LVMBaseInfo.h"
#include "MCTargetDesc/LVMFixupKinds.h"
#include "MCTargetDesc/LVMMCTargetDesc.h"

#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCSection.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/ErrorHandling.h"
#include <list>

namespace llvm
{
    class LVMELFObjectWriter : public MCELFObjectTargetWriter {
    public:
        LVMELFObjectWriter(uint8_t OSABI);

        ~LVMELFObjectWriter() override;

        unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                            const MCFixup &Fixup, bool IsPCRel) const override;
        bool needsRelocateWithSymbol(const MCSymbol &Sym,
                                    unsigned Type) const override;
    };
    std::unique_ptr<MCObjectTargetWriter> createLVMELFObjectWriter(const Triple &TT);

}
#endif