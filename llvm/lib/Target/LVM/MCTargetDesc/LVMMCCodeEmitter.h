#ifndef LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMMCCOEEMITTER_H
#define LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMMCCOEEMITTER_H

#include "LVMInstrInfo.h"
#include "llvm/MC/MCCodeEmitter.h"
#include <cstdint>

using namespace llvm;

namespace llvm {

    class MCContext;
    class MCExpr;
    class MCInst;
    class MCInstrInfo;
    class MCFixup;
    class MCOperand;
    class MCSubtargetInfo;
    class raw_ostream;

    class LVMMCCodeEmitter : public MCCodeEmitter {
        const MCInstrInfo &MCII;
        MCContext &Ctx;
        bool IsLittleEndian;
    public:
        LVMMCCodeEmitter(const MCInstrInfo &mcii, MCContext &Ctx_);
        LVMMCCodeEmitter(const LVMMCCodeEmitter &) = delete;
        LVMMCCodeEmitter &operator=(const LVMMCCodeEmitter &) = delete;
        ~LVMMCCodeEmitter() override = default;


        void getBinaryCodeForInstr(const MCInst &MI, SmallVectorImpl<MCFixup> &Fixups,
          APInt &Inst,
          APInt &Scratch,
          const MCSubtargetInfo &STI) const;

        void EmitByte(unsigned char C, raw_ostream &OS) const;

        void EmitInstruction(APInt Val, unsigned Size, raw_ostream &OS) const;

        void encodeInstruction(const MCInst &MI, raw_ostream &OS,
                              SmallVectorImpl<MCFixup> &Fixups,
                              const MCSubtargetInfo &STI) const override;

        // TableGen'erated function for getting the binary encoding
        // for an instruction.

        // Return binary encoding of the branch target operand,
        // such as BEQ, BNE. If the machine operand requires relocation,
        // record the relocation and return zero.
        unsigned getBranch16TargetOpValue(const MCInst &MI, unsigned OpNo,
                                          SmallVectorImpl<MCFixup> &Fixups,
                                          const MCSubtargetInfo &STI) const;

        // Return binary encoding of the branch target operand,
        // such as JMP. If the machine operand requires relocation,
        // record the relocation and return zero.
        unsigned getBranch24TargetOpValue(const MCInst &MI, unsigned OpNo,
                                          SmallVectorImpl<MCFixup> &Fixups,
                                          const MCSubtargetInfo &STI) const;

        // Return binary encoding of the jump target operand,
        // such as JSUB #function_addr. If the machine operand requires relocation,
        // record the relocation and return zero.
        void getJumpTargetOpValue(const MCInst &MI, unsigned OpNo, APInt Op,
                                      SmallVectorImpl<MCFixup> &FIxups,
                                      const MCSubtargetInfo &STI) const;

        // Return binary encoding of operand.
        // If the machine operand requires relocation,
        // record the relocation and return zero.
        void getMachineOpValue(const MCInst &MI, const MCOperand &MO, APInt& OpValue,
                                  SmallVectorImpl<MCFixup> &Fixups,
                                  const MCSubtargetInfo &STI) const;

        void getMemEncoding(const MCInst &MI, unsigned OpNo, APInt& OpValue,
                                SmallVectorImpl<MCFixup> &Fixups,
                                const MCSubtargetInfo &STI) const;

        unsigned getExprOpValue(const MCExpr *Expr, SmallVectorImpl<MCFixup> &Fixups,
                                const MCSubtargetInfo &STI) const;
    }; // class LVMMCCodeEmitter
} // namespace llvm

#endif
