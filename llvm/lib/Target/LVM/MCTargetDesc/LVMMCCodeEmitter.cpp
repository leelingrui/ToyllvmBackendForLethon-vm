#include "LVMMCCodeEmitter.h"

#include "MCTargetDesc/LVMBaseInfo.h"
#include "MCTargetDesc/LVMFixupKinds.h"
#include "MCTargetDesc/LVMMCExpr.h"
#include "MCTargetDesc/LVMMCTargetDesc.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/raw_ostream.h"


#define DEBUG_TYPE "mccodeemitter"

namespace llvm {

extern const MCInstrDesc LVMInsts[];
MCCodeEmitter *createLVMMCCodeEmitter(const MCInstrInfo &MCII,
                                               MCContext &Ctx) {
  return new LVMMCCodeEmitter(MCII, Ctx);
}
} // namespace llvm

llvm::LVMMCCodeEmitter::LVMMCCodeEmitter(const MCInstrInfo &mcii,
                                         MCContext &Ctx_) 
      : MCII(mcii), Ctx(Ctx_), IsLittleEndian(true) {}

void LVMMCCodeEmitter::EmitByte(unsigned char C, raw_ostream &OS) const {
  OS << (char)C;
}

void LVMMCCodeEmitter::EmitInstruction(APInt Val, unsigned Size, raw_ostream &OS) const
{
    // Output the instruction encoding in little endian byte order.
    const uint8_t *data = reinterpret_cast<const uint8_t*>(Val.getRawData());
    for (unsigned i = 0; i < Size; ++i) {
        EmitByte(data[i], OS);
    }
}

// Emit the instruction.
void LVMMCCodeEmitter::encodeInstruction(const MCInst &MI, raw_ostream &OS,
                                          SmallVectorImpl<MCFixup> &Fixups,
                                          const MCSubtargetInfo &STI) const
{
    LLVM_DEBUG(errs() << "Starting Emit" << MI << "\n");
    APInt EncodedInst;
    APInt Op;
    // Check for unimplemented opcodes.
    unsigned Opcode = MI.getOpcode();
    //   if ((Opcode != LVM::NOP) && !Binary)
    //     llvm_unreachable("unimplemented opcode in encodeInstruction()");
    getBinaryCodeForInstr(MI, Fixups, EncodedInst, Op, STI);
    uint64_t Size = LVMInsts[Opcode].Size;
    // const MCInstrDesc &Desc = MCII.get(MI.getOpcode());
    // uint64_t TSFlags = Desc.TSFlags;
  
    // // Pseudo instruction don't get encoded
    // // and shouldn't be here in the first place.
    // if ((TSFlags & LVMII::FrmMask) == LVMII::Pseudo)
    //   llvm_unreachable("Pseudo opcode found in encodeInstruction()");

    EmitInstruction(std::move(EncodedInst), Size, OS);
}

/// getBranch16TargetOpValue - Return binary encoding of the branch 24bits
/// target operand. If the machine operand requires relocation,
/// record the relocation and return zero.
unsigned LVMMCCodeEmitter::getBranch16TargetOpValue(const MCInst &MI, unsigned OpNo,
                         SmallVectorImpl<MCFixup> &Fixups,
                         const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);

  // If the destination is an immediate, we have nothing to do.
  if (MO.isImm()) return MO.getImm();
  assert(MO.isExpr() && "getBranch16TargetOpValue expects only expressions");

  const MCExpr *Expr = MO.getExpr();
  Fixups.push_back(MCFixup::create(0, Expr,
                                   MCFixupKind(LVM::fixup_LVM_PC8)));
  return 0;
}

/// getBranch24TargetOpValue - Return binary encoding of the branch 24bits
/// target operand. If the machine operand requires relocation,
/// record the relocation and return zero.
// unsigned LVMMCCodeEmitter::
// getBranch24TargetOpValue(const MCInst &MI, unsigned OpNo,
//                          SmallVectorImpl<MCFixup> &Fixups,
//                          const MCSubtargetInfo &STI) const {
//   const MCOperand &MO = MI.getOperand(OpNo);

//   // If the destination is an immediate, we have nothing to do.
//   if (MO.isImm()) return MO.getImm();
//   assert(MO.isExpr() && "getBranch24TargetOpValue expects only expressions");

//   const MCExpr *Expr = MO.getExpr();
//   Fixups.push_back(MCFixup::create(0, Expr,
//                                    MCFixupKind(LVM::fixup_LVM_PC24)));
//   return 0;
// }

/// getJumpTargetOpValue - Return binary encoding of the jump
/// target operand, such as JSUB..
/// If the machine operand requires relocation,
/// record the relocation and return zero.
void LVMMCCodeEmitter::getJumpTargetOpValue(const MCInst &MI, unsigned OpNo, APInt Op,
                     SmallVectorImpl<MCFixup> &Fixups,
                     const MCSubtargetInfo &STI) const {
  unsigned Opcode = MI.getOpcode();
  const MCOperand &MO = MI.getOperand(OpNo);
  // If the destination is an immediate, we have nothing to do.
  if (MO.isImm()) 
  {
    Op = MO.getImm();
  }
  assert(MO.isExpr() && "getJumpTargetOpValue expects only expressions");

  const MCExpr *Expr = MO.getExpr();
  if (/* Opcode == LVM::JMP || Opcode == LVM::JSUB */ Opcode == LVM::CALLI)
    Fixups.push_back(MCFixup::create(0, Expr, MCFixupKind(LVM::fixup_LVM_64)));
  else
    llvm_unreachable("unexpect opcode in getJumpAbsoluteTargetOpValue()");
}

unsigned LVMMCCodeEmitter::getExprOpValue(const MCExpr *Expr,
                                           SmallVectorImpl<MCFixup> &Fixups,
                                           const MCSubtargetInfo &STI) const {
  MCExpr::ExprKind Kind = Expr->getKind();
  if (Kind == MCExpr::Constant) {
    return cast<MCConstantExpr>(Expr)->getValue();
  }

  if (Kind == MCExpr::Binary) {
    unsigned Res = getExprOpValue(cast<MCBinaryExpr>(Expr)->getLHS(), Fixups, STI);
    Res += getExprOpValue(cast<MCBinaryExpr>(Expr)->getRHS(), Fixups, STI);
    return Res;
  }

  if (Kind == MCExpr::Target) {
    const LVMMCExpr *LVMExpr = cast<LVMMCExpr>(Expr);

    LVM::Fixups FixupKind = LVM::Fixups(0);
    switch(LVMExpr->getKind()) {
    default: llvm_unreachable("Unsupported fixup kind for target expression!");
    // case LVMMCExpr::CEK_GPREL:
    //   FixupKind = LVM::fixup_LVM_GPREL16;
    //   break;
    case LVMMCExpr::CEK_GOT_CALL:
      FixupKind = LVM::fixup_LVM_64;
      break;
    // case LVMMCExpr::CEK_GOT:
    //   FixupKind = LVM::fixup_LVM_GOT;
    //   break;
    // case LVMMCExpr::CEK_ABS_HI:
    //   FixupKind = LVM::fixup_LVM_HI16;
    //   break;
    // case LVMMCExpr::CEK_ABS_LO:
    //   FixupKind = LVM::fixup_LVM_LO16;
    //   break;
    // case LVMMCExpr::CEK_GOT_HI16:
    //   FixupKind = LVM::fixup_LVM_GOT_HI16;
    //   break;
    // case LVMMCExpr::CEK_GOT_LO16:
    //   FixupKind = LVM::fixup_LVM_GOT_LO16;
    //   break;
    }
    Fixups.push_back(MCFixup::create(0, LVMExpr, MCFixupKind(FixupKind)));
    return 0;
  }

  // All of the information is in the fixup.
  return 0;
}

void LVMMCCodeEmitter::getMachineOpValue(const MCInst &MI, const MCOperand &MO, APInt& OpValue,
                  SmallVectorImpl<MCFixup> &Fixups, const MCSubtargetInfo &STI) const {
    if (MO.isReg()) {
        unsigned Reg = MO.getReg();
        OpValue |= Ctx.getRegisterInfo()->getEncodingValue(Reg);
        return;
    } else if (MO.isImm()) {
        OpValue |= static_cast<unsigned>(MO.getImm());
        return;
    } else if (MO.isSFPImm()) {
        OpValue |= static_cast<unsigned>(APFloat(static_cast<double>(MO.getSFPImm())).bitcastToAPInt().getHiBits(32).getLimitedValue());
        return;
    }

    // MO must be an Expr.
    assert(MO.isExpr());
    OpValue |= getExprOpValue(MO.getExpr(), Fixups, STI);
}

void LVMMCCodeEmitter::getMemEncoding(const MCInst &MI, unsigned OpNo, APInt& OpVal,
                                           SmallVectorImpl<MCFixup> &Fixups,
                                           const MCSubtargetInfo &STI) const {
  // Base register is encoded in bits 10-15, offset is encoded in bits 16-23.
  assert(MI.getOperand(OpNo).isReg());
  getMachineOpValue(MI, MI.getOperand(OpNo + 1), OpVal, Fixups, STI);
  OpVal <<= 8;
  getMachineOpValue(MI, MI.getOperand(OpNo), OpVal, Fixups, STI);
}

#include "LVMGenMCCodeEmitter.inc"