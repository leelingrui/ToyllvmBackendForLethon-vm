#include "LVMELFObjectWriter.h"

using namespace llvm;
LVMELFObjectWriter::LVMELFObjectWriter(uint8_t OSABI)
  : MCELFObjectTargetWriter(/*_is64Bit=false*/true, OSABI, ELF::EM_LVM,
                            /*HasRelocationAddend*/true) {}

LVMELFObjectWriter::~LVMELFObjectWriter() {}

unsigned LVMELFObjectWriter::getRelocType(MCContext &Ctx,
                                           const MCValue &Target,
                                           const MCFixup &Fixup,
                                           bool IsPCRel) const {
  // determine the type of the relocation
  unsigned Type = (unsigned)ELF::R_LVM_NONE;
  unsigned Kind = (unsigned)Fixup.getKind();

  switch(Kind) {
  default:
    llvm_unreachable("invalid fixup kind!");
  case LVM::fixup_LVM_64:
    Type = ELF::R_LVM_64;
    break;
  case LVM::fixup_LVM_PC8:
    Type = ELF::R_LVM_PC8;
    break;
  case LVM::fixup_LVM_GR64REL:
    Type = ELF::R_LVM_REX_GOTPCRELX;
    break;
  }

  return Type;
}

bool LVMELFObjectWriter::needsRelocateWithSymbol(const MCSymbol &Sym,
                                                  unsigned Type) const {
  // FIXME: This is extremely conservative. This really needs to use a
  // whitelist with a clear explanation for why each realocation needs to
  // point to the symbol, not to the section.
  switch (Type) {
  default:
    return true;

  case ELF::R_LVM_GOT64:
    return true;

  // These relocations might be paired with another relocation. The pairing is
  // done by the static linker by matching the symbol. Since we only see one
  // relocation at a time, we have to force them to relocation with a symbol to
  // avoid ending up with a pair where one points to a section and another
  // points to a symbol.
  case ELF::R_LVM_64:
    return true;

  case ELF::R_LVM_REX_GOTPCRELX:
    return false;
  }
}

std::unique_ptr<MCObjectTargetWriter> llvm::createLVMELFObjectWriter(const Triple &TT) {
  uint8_t OSABI = MCELFObjectTargetWriter::getOSABI(TT.getOS());
  return std::make_unique<LVMELFObjectWriter>(OSABI);
}
