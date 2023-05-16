#include "LVM.h"
#include "llvm/MC/MCInst.h"
#include "MCTargetDesc/LVMMCTargetDesc.h"
#include "LVMRegisterInfo.h"
#include "LVMSubtarget.h"
#include "llvm/MC/MCDisassembler/MCDisassembler.h"
#include "llvm/MC/MCDecoderOps.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/MathExtras.h"
#include "TargetInfo/LVMTargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
using namespace llvm;

#define DEBUG_TYPE "lvm-disassembler"

namespace llvm
{
    extern const MCInstrDesc LVMInsts[];
} // namespace llvm


typedef MCDisassembler::DecodeStatus DecodeStatus;
namespace {

/// LVMdisassemblerBase - a disassembler class for LVM.
class LVMDisassemblerBase : public MCDisassembler {
public:
    LVMDisassemblerBase(const MCSubtargetInfo &STI, MCContext &Ctx,
                        bool bigEndian) :
        MCDisassembler(STI, Ctx),
        IsBigEndian(bigEndian) {}

    virtual ~LVMDisassemblerBase() {}

protected:
  bool IsBigEndian;
};

/// LVMDisassembler - a disassembler class for LVM32.
class LVMDisassembler : public LVMDisassemblerBase {
public:
  LVMDisassembler(const MCSubtargetInfo &STI, MCContext &Ctx, bool bigEndian)
    : LVMDisassemblerBase(STI, Ctx, bigEndian) {}

  DecodeStatus getInstruction(MCInst &Inst, uint64_t &Size,
                              ArrayRef<uint8_t> Bytes, uint64_t Address,
                              raw_ostream &CStream) const override;
};

} // end of anonymous namespace

// Decoder tables for GPR register

static const unsigned CPURegsTable[] = {
LVM::IP, LVM::RAX, LVM::RBX, LVM::RCX,
LVM::RDX, LVM::RFLAGS, LVM::RSP, LVM::RBP,
LVM::RCS, LVM::RDS, LVM::RES, LVM::RFS,
LVM::RGS
};

static DecodeStatus DecodeGR8RegisterClass(MCInst &Inst, unsigned RegNo, uint64_t Address, const void *Decoder);
static DecodeStatus DecodeGR16RegisterClass(MCInst &Inst, unsigned RegNo, uint64_t Address, const void *Decoder);
static DecodeStatus DecodeGR32RegisterClass(MCInst &Inst, unsigned RegNo, uint64_t Address, const void *Decoder);
static DecodeStatus DecodeGR64RegisterClass(MCInst &Inst, unsigned RegNo, uint64_t Address, const void *Decoder);

// static DecodeStatus DecodeCPURegsRegisterClass(MCInst &Inst,
//                                                unsigned RegNo,
//                                                uint64_t Address,
//                                                const void *Decoder);
// static DecodeStatus DecodeGPROutRegisterClass(MCInst &Inst,
//                                               unsigned RegNo,
//                                               uint64_t Address,
//                                               const void *Decoder);
// static DecodeStatus DecodeSRRegisterClass(MCInst &Inst,
//                                           unsigned RegNo,
//                                           uint64_t Address,
//                                           const void *Decoder);
// static DecodeStatus DecodeC0RegsRegisterClass(MCInst &Inst,
//                                               unsigned RegNo,
//                                               uint64_t Address,
//                                               const void *Decoder);
// static DecodeStatus DecodeBranch16Target(MCInst &Inst,
//                                          unsigned Insn,
//                                          uint64_t Address,
//                                          const void *Decoder);
// static DecodeStatus DecodeBranch24Target(MCInst &Inst,
//                                          unsigned Insn,
//                                          uint64_t Address,
//                                          const void *Decoder);
static DecodeStatus DecodeJumpTarget(MCInst &Inst,
                                     APInt Insn,
                                     uint64_t Address,
                                     const void *Decoder);
// static DecodeStatus DecodeJumpFR(MCInst &Inst,
//                                  unsigned Insn,
//                                  uint64_t Address,
//                                  const void *Decoder);
// static DecodeStatus DecodeMem(MCInst &Inst,
//                               unsigned Insn,
//                               uint64_t Address,
//                               const void *decoder);
// static DecodeStatus DecodeSimm16(MCInst &Inst,
//                                  unsigned Insn,
//                                  uint64_t Address,
//                                  const void *Decoder);

namespace llvm {
extern Target TheLVMelTarget, TheLVMTarget, TheLVM64Target,
              TheLVM64elTarget;
}

static MCDisassembler *createLVMDisassembler(
    const Target &T,
    const MCSubtargetInfo &STI,
    MCContext &Ctx) {
  return new LVMDisassembler(STI, Ctx, true);
}

// static MCDisassembler *createLVMelDisassembler(
//     const Target &T,
//     const MCSubtargetInfo &STI,
//     MCContext &Ctx) {
//   return new LVMDisassembler(STI, Ctx, false);
// }

extern "C" void LLVMInitializeLVMDisassembler() {
  // Register the disassembler.
  TargetRegistry::RegisterMCDisassembler(getLVMTarget(),
                                         createLVMDisassembler);
//   TargetRegistry::RegisterMCDisassembler(getTheLVMelTarget(),
//                                          createLVMelDisassembler);
}
#include "LVMGenDisassemblerTables.inc"

// /// Read four bytes from the ArrayRef and return 32 bit word sorted
// /// according to the given endianess
// static DecodeStatus readInstruction32(ArrayRef<uint8_t> Bytes, uint64_t Address,
//                                       uint64_t &Size, uint32_t &Insn,
//                                       bool IsBigEndian) {
//   // We want to read exactly 4 Bytes of data.
//   if (Bytes.size() < 4) {
//     Size = 0;
//     return MCDisassembler::Fail;
//   }

//   if (IsBigEndian) {
//     // Encoded as a big-endian 32-bit word in the stream.
//     Insn = (Bytes[3] << 0)  |
//            (Bytes[2] << 8)  |
//            (Bytes[1] << 16) |
//            (Bytes[0] << 24);
//   } else {
//     // Encoded as a little-endian 32-bit word in the stream.
//     Insn = (Bytes[0] << 0)  |
//            (Bytes[1] << 8)  |
//            (Bytes[2] << 16) |
//            (Bytes[3] << 24);
//   }

//   return MCDisassembler::Success;
// }

DecodeStatus LVMDisassembler::getInstruction(MCInst &Instr, uint64_t &Size,
                                              ArrayRef<uint8_t> Bytes,
                                              uint64_t Address,
                                              raw_ostream &CStream) const
{
    DecodeStatus Result;
    const uint8_t *DecoderTable = DecoderTable16;
    unsigned Opcode = Bytes[0] & 0x3f;
    switch (Opcode)
    {
    case 1:case 2: case 3: case 4: case 28: case 29:
        DecoderTable = DecoderTable32;
        Size = 4;
        break;
    case 7: case 8:
        DecoderTable = DecoderTable16;
        Size = 2;
        break;
    case 33:
        DecoderTable = DecoderTable24;
        Size = 3;
        break;
    case 9: case 10:
        if ((Bytes[1] & 0xC0) >> 6 > 1)
        {
            DecoderTable = DecoderTable32;
            Size = 4;
        }
        else
        {
            DecoderTable = DecoderTable24;
            Size = 3;
        }
        break;
    case 31:
        DecoderTable = DecoderTable8;
        Size = 1;
        break;
    case 30:
        if ((Bytes[1] & 0xC0) >> 6 > 0)
        {
            DecoderTable = DecoderTable16;
            Size = 2;
        }
        else
        {
            DecoderTable = DecoderTable72;
            Size = 9;
        }
        break;
    default:
        llvm_unreachable("impossiable Instruction Size!");
    }
    APInt Insn = APInt::getZero(Size * 8);
    for (int i = 0; i < Size; i++)
    {
        const_cast<char*>(reinterpret_cast<const char*>(Insn.getRawData()))[i] = Bytes[i];
    } 
  // Result = readInstruction32(Bytes, Address, Size, Insn, IsBigEndian);

//   if (Result == MCDisassembler::Fail)
//     return Result;

  // Calling the auto-generated decoder function
    Result = decodeInstruction(DecoderTable, Instr, Insn, Address, this, STI);
    Instr.dump();
    return Result;
}

// static DecodeStatus DecodeGPROutRegisterClass(MCInst &Inst,
//                                               unsigned RegNo,
//                                               uint64_t Address,
//                                               const void *Decoder) {
//   return DecodeCPURegsRegisterClass(Inst, RegNo, Address, Decoder);
// }

// static DecodeStatus DecodeSRRegisterClass(MCInst &Inst,
//                                           unsigned RegNo,
//                                           uint64_t Address,
//                                           const void *Decoder) {
//   return DecodeCPURegsRegisterClass(Inst, RegNo, Address, Decoder);
// }

// static DecodeStatus DecodeC0RegsRegisterClass(MCInst &Inst,
//                                               unsigned RegNo,
//                                               uint64_t Address,
//                                               const void *Decoder) {
//   if (RegNo > 1)
//     return MCDisassembler::Fail;

//   Inst.addOperand(MCOperand::createReg(C0RegsTable[RegNo]));
//   return MCDisassembler::Success;
// }

// static DecodeStatus DecodeBranch16Target(MCInst &Inst,
//                                          unsigned Insn,
//                                          uint64_t Address,
//                                          const void *Decoder) {
//   int BranchOffset = fieldFromInstruction(Insn, 0, 16);
//   if (BranchOffset > 0x8fff)
//     BranchOffset = -1 * (0x10000 - BranchOffset);

//   Inst.addOperand(MCOperand::createImm(BranchOffset));
//   return MCDisassembler::Success;
// }

// static DecodeStatus DecodeBranch24Target(MCInst &Inst,
//                                          unsigned Insn,
//                                          uint64_t Address,
//                                          const void *Decoder) {
//   int BranchOffset = fieldFromInstruction(Insn, 0, 24);
//   if (BranchOffset > 0x8ffff)
//     BranchOffset = -1 * (0x10000000 - BranchOffset);

//   Inst.addOperand(MCOperand::createReg(LVM::SW));
//   Inst.addOperand(MCOperand::createImm(BranchOffset));
//   return MCDisassembler::Success;
// }
static DecodeStatus DecodeJumpTarget(MCInst &Inst,
                                     APInt Insn,
                                     uint64_t Address,
                                     const void *Decoder) {
  unsigned long JumpOffset = fieldFromInstruction(Insn, 0, 24);
  Inst.addOperand(MCOperand::createImm(JumpOffset));
  return MCDisassembler::Success;
}

// static DecodeStatus DecodeJumpFR(MCInst &Inst,
//                                  unsigned Insn,
//                                  uint64_t Address,
//                                  const void *Decoder) {
//   int Reg = (int)fieldFromInstruction(Insn, 20, 4);
//   Inst.addOperand(MCOperand::createReg(CPURegsTable[Reg]));
//   if (CPURegsTable[Reg] == LVM::LR)
//     Inst.setOpcode(LVM::RET);
//   else
//     Inst.setOpcode(LVM::JR);
//   return MCDisassembler::Success;
// }

// static DecodeStatus DecodeMem(MCInst &Inst,
//                               unsigned Insn,
//                               uint64_t Address,
//                               const void *decoder) {
//   int Offset = SignExtend32<16>(Insn & 0xffff);
//   int Reg = (int)fieldFromInstruction(Insn, 20, 4);
//   int Base = (int)fieldFromInstruction(Insn, 16, 4);

//   Inst.addOperand(MCOperand::createReg(CPURegsTable[Reg]));
//   Inst.addOperand(MCOperand::createReg(CPURegsTable[Base]));
//   Inst.addOperand(MCOperand::createImm(Offset));

//   return MCDisassembler::Success;
// }

// static DecodeStatus DecodeSimm16(MCInst &Inst,
//                                  unsigned Insn,
//                                  uint64_t Address,
//                                  const void *Decoder) {
//   Inst.addOperand(MCOperand::createImm(SignExtend32<16>(Insn)));
//   return MCDisassembler::Success;
// }

// static DecodeStatus DecodeCPURegsRegisterClass(MCInst &Inst,
//                                                unsigned RegNo,
//                                                uint64_t Address,
//                                                const void *Decoder) {
//   if (RegNo > 15)
//     return MCDisassembler::Fail;

//   Inst.addOperand(MCOperand::createReg(CPURegsTable[RegNo]));
//   return MCDisassembler::Success;
// }

DecodeStatus DecodeGR8RegisterClass(MCInst &Inst, unsigned RegNo,
                                    uint64_t Address, const void *Decoder) {
    if (RegNo > 4) return MCDisassembler::Fail;
    Inst.addOperand(MCOperand::createReg(LVMMCRegisterClasses[0].RegSet[RegNo]));
    return MCDisassembler::Success;
}

DecodeStatus DecodeGR16RegisterClass(MCInst &Inst, unsigned RegNo,
                                     uint64_t Address, const void *Decoder) {
    if (RegNo > 4) return MCDisassembler::Fail;
    Inst.addOperand(MCOperand::createReg(LVMMCRegisterClasses[1].RegSet[RegNo]));
    return MCDisassembler::Success;
}

DecodeStatus DecodeGR32RegisterClass(MCInst &Inst, unsigned RegNo,
                                     uint64_t Address, const void *Decoder) {
    if (RegNo > 4) return MCDisassembler::Fail;
    Inst.addOperand(MCOperand::createReg(LVMMCRegisterClasses[2].RegSet[RegNo]));
    return MCDisassembler::Success;
}
DecodeStatus DecodeGR64RegisterClass(MCInst &Inst, unsigned RegNo,
                                     uint64_t Address, const void *Decoder) {
    if (RegNo > 12) return MCDisassembler::Fail;
    Inst.addOperand(MCOperand::createReg(LVMMCRegisterClasses[3].RegSet[RegNo]));
    return MCDisassembler::Success;
}
