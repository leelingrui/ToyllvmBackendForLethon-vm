#include "OutputSections.h"
#include "Symbols.h"
#include "SyntheticSections.h"
#include "Target.h"
#include "lld/Common/ErrorHandler.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Support/Endian.h"

using namespace llvm;
using namespace llvm::object;
using namespace llvm::support::endian;
using namespace llvm::ELF;
using namespace lld;
using namespace lld::elf;

namespace {
class LVM : public TargetInfo {
public:
    LVM();
    // int getTlsGdRelaxSkip(RelType type) const override;
    RelExpr getRelExpr(RelType type, const Symbol &s,
                        const uint8_t *loc) const override;
    // RelType getDynRel(RelType type) const override;
    // void writeGotPltHeader(uint8_t *buf) const override;
    // void writeGotPlt(uint8_t *buf, const Symbol &s) const override;
    // void writeIgotPlt(uint8_t *buf, const Symbol &s) const override;
    // void writePltHeader(uint8_t *buf) const override;
    // void writePlt(uint8_t *buf, const Symbol &sym,
    //                 uint64_t pltEntryAddr) const override;
    void relocate(uint8_t *loc, const Relocation &rel,
                    uint64_t val) const override;
    // int64_t getImplicitAddend(const uint8_t *buf, RelType type) const override;
    // void applyJumpInstrMod(uint8_t *loc, JumpModType type,
    //                         unsigned size) const override;

    // RelExpr adjustGotPcExpr(RelType type, int64_t addend,
    //                         const uint8_t *loc) const override;
    // void relaxGot(uint8_t *loc, const Relocation &rel,
    //                 uint64_t val) const override;
    // void relaxTlsGdToIe(uint8_t *loc, const Relocation &rel,
    //                     uint64_t val) const override;
    // void relaxTlsGdToLe(uint8_t *loc, const Relocation &rel,
    //                     uint64_t val) const override;
    // void relaxTlsIeToLe(uint8_t *loc, const Relocation &rel,
    //                     uint64_t val) const override;
    // void relaxTlsLdToLe(uint8_t *loc, const Relocation &rel,
    //                     uint64_t val) const override;
    // bool adjustPrologueForCrossSplitStack(uint8_t *loc, uint8_t *end,
    //                                         uint8_t stOther) const override;
    // bool deleteFallThruJmpInsn(InputSection &is, InputFile *file,
    //                             InputSection *nextIS) const override;
};

    static const std::vector<std::vector<uint8_t>> nopInstructions = {
        {0x22}
    };

    void LVM::relocate(uint8_t *loc, const Relocation &rel, uint64_t val) const
    {
        switch (rel.type)
        {
        case R_LVM_64:
            write64le(loc + 1, val);
            break;
        case R_LVM_PC8:
            checkInt(loc, val, 8, rel);
            *loc = val;
            break;
        default:
            llvm_unreachable("unknown relocation");
            break;
        }
    }
    LVM::LVM()
    {
        copyRel = R_LVM_COPY;
        gotRel = R_LVM_GLOB_DAT;
        pltRel = R_LVM_JUMP_SLOT;
        relativeRel = R_LVM_RELATIVE;
        iRelativeRel = R_LVM_IRELATIVE;
        symbolicRel = R_LVM_64;
        tlsDescRel = R_LVM_TLSDESC;
        tlsGotRel = R_LVM_TPOFF64;
        tlsModuleIndexRel = R_LVM_DTPMOD64;
        tlsOffsetRel = R_LVM_DTPOFF64;
        gotBaseSymInGotPlt = true;
        gotEntrySize = 8;
        pltHeaderSize = 16;
        pltEntrySize = 16;
        ipltEntrySize = 16;
        trapInstr = {0xcc, 0xcc, 0xcc, 0xcc}; // 0xcc = INT3
        nopInstrs = nopInstructions;

        // Align to the large page size (known as a superpage or huge page).
        // FreeBSD automatically promotes large, superpage-aligned allocations.
        defaultImageBase = 0x200000;
    }
    RelExpr LVM::getRelExpr(RelType type, const Symbol &s,
                            const uint8_t *loc) const
    {
        switch (type)
        {
        case R_LVM_8:
        case R_LVM_16:
        case R_LVM_32:
        case R_LVM_32S:
        case R_LVM_64:
            return R_ABS;
        case R_LVM_GOTOFF64:
            return R_GOTPLTREL;
        case R_LVM_PC8:
        case R_LVM_PC16:
        case R_LVM_PC32:
        case R_LVM_PC64:
            return R_PC;
        default:
            error(getErrorLocation(loc) + "unknown relocation (" + Twine(type) +
                ") against symbol " + toString(s));
            return R_NONE;
        }
    }
}
static TargetInfo *getTargetInfo() {
  static LVM t;
  return &t;
}

TargetInfo *elf::getLVMTargetInfo() { return getTargetInfo(); }