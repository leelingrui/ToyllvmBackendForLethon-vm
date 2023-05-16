//===-- LVMMCExpr.h - LVM specific MC expression classes ------*- C++ -*-===//
//
//                    The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMMCEXPR_H
#define LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMMCEXPR_H

#include "llvm/MC/MCAsmLayout.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/Casting.h"

namespace llvm {

class LVMMCExpr : public MCTargetExpr {
public:
  enum LVMExprKind {
    CEK_None,
    CEK_ABS_HI,
    CEK_ABS_LO,
    CEK_CALL_HI16,
    CEK_CALL_LO16,
    CEK_DTP_HI,
    CEK_DTP_LO,
    CEK_GOT,
    CEK_GOTTPREL,
    CEK_GOT_CALL,
    CEK_GOT_DISP,
    CEK_GOT_HI16,
    CEK_GOT_LO16,
    CEK_GPREL,
    CEK_TLSGD,
    CEK_TLSLDM,
    CEK_TP_HI,
    CEK_TP_LO,
    CEK_Special,
  };

private:
  const LVMExprKind Kind;
  const MCExpr *Expr;

  explicit LVMMCExpr(LVMExprKind Kind, const MCExpr *Expr)
    : Kind(Kind), Expr(Expr) {}

public:
  static const LVMMCExpr *create(LVMExprKind Kind, const MCExpr *Expr,
                                  MCContext &Ctx);
  static const LVMMCExpr *create(const MCSymbol *Symbol,
                                  LVMMCExpr::LVMExprKind Kind, MCContext &Ctx);
  static const LVMMCExpr *createGpOff(LVMExprKind Kind, const MCExpr *Expr,
                                       MCContext &Ctx);

  /// Get the kind of this expression.
  LVMExprKind getKind() const { return Kind; }

  /// Get the child of this expression.
  const MCExpr *getSubExpr() const { return Expr; }

  void printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const override;
  bool evaluateAsRelocatableImpl(MCValue &Res, const MCAsmLayout *Layout,
                                 const MCFixup *Fixup) const override;
  void visitUsedExpr(MCStreamer &Streamer) const override;
  MCFragment *findAssociatedFragment() const override {
    return getSubExpr()->findAssociatedFragment();
  }

  void fixELFSymbolsInTLSFixups(MCAssembler &Asm) const override;

  static bool classof(const MCExpr *E) {
    return E->getKind() == MCExpr::Target;
  }

  bool isGpOff(LVMExprKind &Kind) const;
  bool isGpOff() const {
    LVMExprKind Kind;
    return isGpOff(Kind);
  }
};
} // end namespace llvm

#endif
