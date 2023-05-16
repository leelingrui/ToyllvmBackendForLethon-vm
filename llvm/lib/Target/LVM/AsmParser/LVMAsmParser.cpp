#include "LVM.h"

#include "MCTargetDesc/LVMMCExpr.h"
#include "MCTargetDesc/LVMMCTargetDesc.h"
#include "LVMSubtarget.h"
#include "LVMRegisterInfo.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstBuilder.h"
#include "llvm/MC/MCParser/MCAsmLexer.h"
#include "llvm/MC/MCParser/MCAsmParser.h"
#include "llvm/MC/MCParser/MCAsmParserExtension.h"
#include "llvm/MC/MCParser/MCParsedAsmOperand.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCValue.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/MC/TargetRegistry.h"
#include "TargetInfo/LVMTargetInfo.h"
using namespace llvm;

#define DEBUG_TYPE "LVM-asm-parser"

namespace {
    class LVMAssemblerOptions {
    public:
        LVMAssemblerOptions() :
            reorder(true), macro(true) {
        }

        bool isReorder() { return reorder; }
        void setReorder() { reorder = true; }
        void setNoreorder() { reorder = false; }

        bool isMacro() { return macro; }
        void setMacro() { macro = true; }
        void setNomacro() { macro = false; }

    private:
        bool reorder;
        bool macro;
    };
}

namespace {
class LVMAsmParser : public MCTargetAsmParser {
    MCAsmParser &Parser;
    LVMAssemblerOptions Options;

#define GET_ASSEMBLER_HEADER
#include "LVMGenAsmMatcher.inc"

    bool MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                                OperandVector &Operands, MCStreamer &Out,
                                uint64_t &ErrorInfo,
                                bool MatchingInlineAsm) override;

    bool ParseRegister(unsigned &RegNo, SMLoc &StartLoc, SMLoc &EndLoc) override;

    bool ParseInstruction(ParseInstructionInfo &Info, StringRef Name,
                          SMLoc NameLoc, OperandVector &Operands) override;

    bool parseMathOperation(StringRef Name, SMLoc NameLoc,
                            OperandVector &Operands);

    bool ParseDirective(AsmToken DirectiveID) override;

    OperandMatchResultTy parseMemOperand(OperandVector &);

    bool ParseOperand(OperandVector &Operands, StringRef Mnemonic);

    OperandMatchResultTy tryParseRegister(unsigned &RegNo, SMLoc &StartLoc, SMLoc &EndLoc) override;

    bool tryParseRegisterOperand(OperandVector &Operands, StringRef Mnemonic);

    bool needsExpansion(MCInst &Inst);

    void expandInstruction(MCInst &Inst, SMLoc IDLoc,
                          SmallVectorImpl<MCInst> &Instructions);
    void expandLoadImm(MCInst &Inst, SMLoc IDLoc,
                      SmallVectorImpl<MCInst> &Instructions);
    void expandLoadAddressImm(MCInst &Inst, SMLoc IDLoc,
                              SmallVectorImpl<MCInst> &Instructions);
    void expandLoadAddressReg(MCInst &Inst, SMLoc IDLoc,
                              SmallVectorImpl<MCInst> &Instructions);
    bool reportParseError(StringRef ErrorMsg);

    bool parseMemOffset(const MCExpr *&Res);
    bool parseRelocOperand(const MCExpr *&Res);

    const MCExpr *evaluateRelocExpr(const MCExpr *Expr, StringRef RelocStr);

    bool parseDirectiveSet();

    bool parseSetAtDirective();
    bool parseSetNoAtDirective();
    bool parseSetMacroDirective();
    bool parseSetNoMacroDirective();
    bool parseSetReorderDirective();
    bool parseSetNoReorderDirective();

    int matchRegisterName(StringRef Symbol);

    int matchRegisterByNumber(unsigned RegNum, StringRef Mnemonic);

    unsigned getReg(int RC, int RegNo);

public:
    LVMAsmParser(const MCSubtargetInfo &sti, MCAsmParser &parser, const MCInstrInfo &MII, const MCTargetOptions &Options) : MCTargetAsmParser(Options, sti, MII), Parser(parser) {
        setAvailableFeatures(ComputeAvailableFeatures(getSTI().getFeatureBits()));
    }

    MCAsmParser &getParser() const { return Parser; }
    MCAsmLexer &getLexer() const { return Parser.getLexer(); }

};
}

namespace {

/// LVMOperand - Instances of this class represent a parsed LVM machine
/// instructions.
class LVMOperand : public MCParsedAsmOperand {
  enum KindTy {
    k_Immediate,
    k_Memory,
    k_Register,
    k_Token
  } Kind;

public:
  LVMOperand(KindTy K) : MCParsedAsmOperand(), Kind(K) {}

  struct Token {
    const char *Data;
    unsigned Length;
  };
  struct PhysRegOp {
    unsigned RegNum;
  };
  struct ImmOp {
    const MCExpr *Val;
  };
  struct MemOp {
    unsigned Base;
    const MCExpr *Off;
  };

  union {
    struct Token Tok;
    struct PhysRegOp Reg;
    struct ImmOp Imm;
    struct MemOp Mem;
  };

  SMLoc StartLoc, EndLoc;

public:
  void addRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getReg()));
  }

  inline bool isImmSExti16i8Value(uint64_t Value) {
    return isInt<8>(Value) ||
          (isUInt<16>(Value) && isInt<8>(static_cast<int16_t>(Value)));
  }

  inline bool isImmSExti32i8Value(uint64_t Value) {
    return isInt<8>(Value) ||
          (isUInt<32>(Value) && isInt<8>(static_cast<int16_t>(Value)));
  }

  inline bool isImmSExti64i8Value(uint64_t Value) {
    return isInt<8>(Value) ||
          (isUInt<64>(Value) && isInt<8>(static_cast<int16_t>(Value)));
  }

  bool isImmSExti16i8()
  {
    if (!isImm())
      return false;

    // If this isn't a constant expr, just assume it fits and let relaxation
    // handle it.
    const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(getImm());
    if (!CE)
      return true;

    // Otherwise, check the value is in a range that makes sense for this
    // extension.
    return isImmSExti16i8Value(CE->getValue());
  }

  bool isImmSExti32i8()
  {
    if (!isImm())
      return false;

    // If this isn't a constant expr, just assume it fits and let relaxation
    // handle it.
    const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(getImm());
    if (!CE)
      return true;

    // Otherwise, check the value is in a range that makes sense for this
    // extension.
    return isImmSExti32i8Value(CE->getValue());
  }

  bool isImmSExti64i8()
  {
    if (!isImm())
      return false;

    // If this isn't a constant expr, just assume it fits and let relaxation
    // handle it.
    const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(getImm());
    if (!CE)
      return true;

    // Otherwise, check the value is in a range that makes sense for this
    // extension.
    return isImmSExti64i8Value(CE->getValue());
  }

  void addExpr(MCInst &Inst, const MCExpr *Expr) const {
    // Add as immediate when possible. Null MCExpr = 0.
    if (Expr == 0)
      Inst.addOperand(MCOperand::createImm(0));
    else if (const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Expr))
      Inst.addOperand(MCOperand::createImm(CE->getValue()));
    else
      Inst.addOperand(MCOperand::createExpr(Expr));
  }

  void addImmOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    const MCExpr *Expr = getImm();
    addExpr(Inst, Expr);
  }

  void addMemOperands(MCInst &Inst, unsigned N) const {
    assert(N == 2 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getMemBase()));

    const MCExpr *Expr = getMemOff();
    addExpr(Inst, Expr);
  }

  bool isReg() const override { return Kind == k_Register; }
  bool isImm() const override { return Kind == k_Immediate; }
  bool isToken() const override { return Kind == k_Token; }
  bool isMem() const override { return Kind == k_Memory; }

  StringRef getToken() const {
    assert(Kind == k_Token && "Invalid access!");
    return StringRef(Tok.Data, Tok.Length);
  }

  unsigned getReg() const override {
    assert(Kind == k_Register && "Invalid access!");
    return Reg.RegNum;
  }

  const MCExpr *getImm() const {
    assert(Kind == k_Immediate && "Invalid access!");
    return Imm.Val;
  }

  unsigned getMemBase() const {
    assert(Kind == k_Memory && "Invalid access!");
    return Mem.Base;
  }

  const MCExpr *getMemOff() const {
    assert(Kind == k_Memory && "Invalid access!");
    return Mem.Off;
  }

  static std::unique_ptr<LVMOperand> CreateToken(StringRef Str, SMLoc S) {
    auto Op = std::make_unique<LVMOperand>(k_Token);
    Op->Tok.Data = Str.data();
    Op->Tok.Length = Str.size();
    Op->StartLoc = S;
    Op->EndLoc = S;
    return Op;
  }

  // Internal constructor for register kinds
  static std::unique_ptr<LVMOperand> CreateReg(unsigned RegNum, SMLoc S,
                                                SMLoc E) {
    auto Op = std::make_unique<LVMOperand>(k_Register);
    Op->Reg.RegNum = RegNum;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static std::unique_ptr<LVMOperand> CreateImm(const MCExpr *Val, SMLoc S,
                                                SMLoc E) {
    auto Op = std::make_unique<LVMOperand>(k_Immediate);
    Op->Imm.Val = Val;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static std::unique_ptr<LVMOperand> CreateMem(unsigned Base, const MCExpr *Off,
                                                SMLoc S, SMLoc E) {
    auto Op = std::make_unique<LVMOperand>(k_Memory);
    Op->Mem.Base = Base;
    Op->Mem.Off = Off;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  /// getStartLoc - Get the location of the first token of this operand.
  SMLoc getStartLoc() const override { return StartLoc; }
  /// getEndLoc - Get the location of the last token of this operand.
  SMLoc getEndLoc() const override { return EndLoc; }

  void print(raw_ostream &OS) const override {
    switch (Kind) {
      case k_Immediate:
        OS << "Imm<" << *Imm.Val << ">";
        break;
      case k_Memory:
        OS << "Mem<";
        OS << Mem.Base;
        OS << ", ";
        OS << *Mem.Off;
        OS << ">";
        break;
      case k_Register:
        OS << "Register<" << Reg.RegNum << ">";
        break;
      case k_Token:
        OS << Tok.Data;
        break;
    }
  }
};
}

void printLVMOperands(OperandVector &Operands) {
  for (size_t i = 0; i < Operands.size(); i++) {
    LVMOperand* op = static_cast<LVMOperand*>(&*Operands[i]);
    assert(op != nullptr);
    LLVM_DEBUG(dbgs() << *op);
  }
  LLVM_DEBUG(dbgs() << "\n");
}

bool LVMAsmParser::needsExpansion(MCInst &Inst) {
  switch(Inst.getOpcode()) {
    // case LVM::LoadImm32Reg:
    // case LVM::LoadAddr32Imm:
    // case LVM::LoadAddr32Reg:
    //   return true;
    default:
      return false;
  }
}

void LVMAsmParser::expandInstruction(MCInst &Inst, SMLoc IDLoc,
                                      SmallVectorImpl<MCInst> &Instructions) {
  // switch(Inst.getOpcode()) {
  // case LVM::LoadImm32Reg:
  //   return expandLoadImm(Inst, IDLoc, Instructions);
  // case LVM::LoadAddr32Imm:
  //   return expandLoadAddressImm(Inst, IDLoc, Instructions);
  // case LVM::LoadAddr32Reg:
  //   return expandLoadAddressReg(Inst, IDLoc, Instructions);
  // }
}

void LVMAsmParser::expandLoadImm(MCInst &Inst, SMLoc IDLoc,
                                  SmallVectorImpl<MCInst> &Instructions) {
  MCInst tmpInst;
  const MCOperand &ImmOp = Inst.getOperand(1);
  assert(ImmOp.isImm() && "expected immediate operand kind");
  const MCOperand &RegOp = Inst.getOperand(0);
  assert(RegOp.isReg() && "expected immediate operand kind");

  int ImmValue = ImmOp.getImm();
  tmpInst.setLoc(IDLoc);
  // if (0 <= ImmValue && ImmValue <= 65535) {
  //   // for 0 <= j <= 65535
  //   // ld d, j -> ori d, $zero, j
  //   tmpInst.setOpcode(LVM::ORi);
  //   tmpInst.addOperand(MCOperand::createReg(RegOp.getReg()));
  //   tmpInst.addOperand(MCOperand::createReg(LVM::ZERO));
  //   tmpInst.addOperand(MCOperand::createImm(ImmValue));
  //   Instructions.push_back(tmpInst);
  // } else if (ImmValue < 0 && ImmValue >= -32768) {
  //   // for -32768 <= j < 0
  //   // ld d, j -> addiu d, $zero, j
  //   tmpInst.setOpcode(LVM::ADDiu);
  //   tmpInst.addOperand(MCOperand::createReg(RegOp.getReg()));
  //   tmpInst.addOperand(MCOperand::createReg(LVM::ZERO));
  //   tmpInst.addOperand(MCOperand::createImm(ImmValue));
  //   Instructions.push_back(tmpInst);
  // } else {
  //   // for any other value of j that is representable as a 32-bit integer.
  //   // ld d, j -> lui d, %hi16(j)
  //   //            ori d, d, %lo16(j)
  //   tmpInst.setOpcode(LVM::LUi);
  //   tmpInst.addOperand(MCOperand::createReg(RegOp.getReg()));
  //   tmpInst.addOperand(MCOperand::createImm((ImmValue & 0xffff0000) >> 16));
  //   Instructions.push_back(tmpInst);
  //   tmpInst.clear();
  //   tmpInst.setOpcode(LVM::ORi);
  //   tmpInst.addOperand(MCOperand::createReg(RegOp.getReg()));
  //   tmpInst.addOperand(MCOperand::createReg(RegOp.getReg()));
  //   tmpInst.addOperand(MCOperand::createImm(ImmValue & 0xffff));
  //   tmpInst.setLoc(IDLoc);
  //   Instructions.push_back(tmpInst);
  // }
}

void LVMAsmParser::expandLoadAddressReg(MCInst &Inst, SMLoc IDLoc,
                                         SmallVectorImpl<MCInst> &Instructions) {
  // MCInst tmpInst;
  // const MCOperand &ImmOp = Inst.getOperand(2);
  // assert(ImmOp.isImm() && "expected immediate operand kind");
  // const MCOperand &SrcRegOp = Inst.getOperand(1);
  // assert(SrcRegOp.isReg() && "expected register operand kind");
  // const MCOperand &DstRegOp = Inst.getOperand(0);
  // assert(DstRegOp.isReg() && "expected register operand kind");
  // int ImmValue = ImmOp.getImm();
  // if (-32768 <= ImmValue && ImmValue <= 32767) {
  //   // for -32768 <= j <= 32767
  //   // ld d, j(s) -> addiu d, s, j
  //   tmpInst.setOpcode(LVM::ADDiu);
  //   tmpInst.addOperand(MCOperand::createReg(DstRegOp.getReg()));
  //   tmpInst.addOperand(MCOperand::createReg(SrcRegOp.getReg()));
  //   tmpInst.addOperand(MCOperand::createImm(ImmValue));
  //   Instructions.push_back(tmpInst);
  // } else {
  //   // for any other value of j that is representable as a 32-bit integer.
  //   // ld d, j(s) -> lui d, %hi16(j)
  //   //               ori d, d, %lo16(j)
  //   //               add d, d, s
  //   tmpInst.setOpcode(LVM::LUi);
  //   tmpInst.addOperand(MCOperand::createReg(DstRegOp.getReg()));
  //   tmpInst.addOperand(MCOperand::createImm((ImmValue & 0xffff0000) >> 16));
  //   Instructions.push_back(tmpInst);
  //   tmpInst.clear();
  //   tmpInst.setOpcode(LVM::ORi);
  //   tmpInst.addOperand(MCOperand::createReg(DstRegOp.getReg()));
  //   tmpInst.addOperand(MCOperand::createReg(DstRegOp.getReg()));
  //   tmpInst.addOperand(MCOperand::createImm(ImmValue & 0xffff));
  //   Instructions.push_back(tmpInst);
  //   tmpInst.clear();
  //   tmpInst.setOpcode(LVM::ADD);
  //   tmpInst.addOperand(MCOperand::createReg(DstRegOp.getReg()));
  //   tmpInst.addOperand(MCOperand::createReg(DstRegOp.getReg()));
  //   tmpInst.addOperand(MCOperand::createReg(SrcRegOp.getReg()));
  //   Instructions.push_back(tmpInst);
  // }
}

void LVMAsmParser::expandLoadAddressImm(MCInst &Inst, SMLoc IDLoc,
    SmallVectorImpl<MCInst> &Instructions) {
  // MCInst tmpInst;
  // const MCOperand &ImmOp = Inst.getOperand(1);
  // assert(ImmOp.isImm() && "expected immediate operand kind");
  // const MCOperand &RegOp = Inst.getOperand(0);
  // assert(RegOp.isReg() && "expected register operand kind");
  // int ImmValue = ImmOp.getImm();
  // if (-32768 <= ImmValue && ImmValue <= 32767) {
  //   // for -32768 <= j <= 32768
  //   // ld d, j -> addiu d, $zero, j
  //   tmpInst.setOpcode(LVM::ADDiu);
  //   tmpInst.addOperand(MCOperand::createReg(RegOp.getReg()));
  //   tmpInst.addOperand(MCOperand::createReg(LVM::ZERO));
  //   tmpInst.addOperand(MCOperand::createImm(ImmValue));
  //   Instructions.push_back(tmpInst);
  // } else {
  //   // for any other value of j that is representable as a 32-bit integer.
  //   // ld d, j -> lui d, %hi16(j)
  //   //            ori d, d, %lo16(j)
  //   tmpInst.setOpcode(LVM::LUi);
  //   tmpInst.addOperand(MCOperand::createReg(RegOp.getReg()));
  //   tmpInst.addOperand(MCOperand::createImm((ImmValue & 0xffff0000) >> 16));
  //   Instructions.push_back(tmpInst);
  //   tmpInst.clear();
  //   tmpInst.setOpcode(LVM::ORi);
  //   tmpInst.addOperand(MCOperand::createReg(RegOp.getReg()));
  //   tmpInst.addOperand(MCOperand::createReg(RegOp.getReg()));
  //   tmpInst.addOperand(MCOperand::createImm(ImmValue & 0xffff));
  //   Instructions.push_back(tmpInst);
  // }
}

bool LVMAsmParser::MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                                            OperandVector &Operands,
                                            MCStreamer &Out,
                                            uint64_t &ErrorInfo,
                                            bool MatchingInlineAsm) {
  printLVMOperands(Operands);
  MCInst Inst;
  unsigned MatchResult = MatchInstructionImpl(Operands, Inst, ErrorInfo,
                                              MatchingInlineAsm);
  switch (MatchResult) {
  default : break;
  case Match_Success : {
    if (needsExpansion(Inst)) {
      SmallVector<MCInst, 4> Instructions;
      expandInstruction(Inst, IDLoc, Instructions);
      for (unsigned i = 0; i < Instructions.size(); i++) {
        Out.emitInstruction(Instructions[i], getSTI());
      }
    } else {
      Inst.setLoc(IDLoc);
      Out.emitInstruction(Inst, getSTI());
    }
    return false;
  }
  case Match_MissingFeature :
    Error(IDLoc, "instruction requires a CPU feature not currently enabled");
    return true;
  case Match_InvalidOperand : {
    SMLoc ErrorLoc = IDLoc;
    if (ErrorInfo != ~0U) {
      if (ErrorInfo >= Operands.size())
        return Error(IDLoc, "too few operands for instruction");

      ErrorLoc = ((LVMOperand &)*Operands[ErrorInfo]).getStartLoc();
      if (ErrorLoc == SMLoc()) ErrorLoc = IDLoc;
    }
    return Error(ErrorLoc, "invalid operand for instruction");
  }
  case Match_MnemonicFail:
    return Error(IDLoc, "invalid instruction");
  }
  return true;
}

int LVMAsmParser::matchRegisterName(StringRef Name) {
  int CC;
  CC = StringSwitch<unsigned>(Name)
    .Case("rax", LVM::RAX)
    .Case("rbx", LVM::RBX)
    .Case("rcx", LVM::RCX)
    .Case("rdx", LVM::RDX)
    .Case("rflags", LVM::RFLAGS)
    .Case("rsp", LVM::RSP)
    .Case("rbp", LVM::RBP)
    .Case("rcs", LVM::RCS)
    .Case("rds", LVM::RDS)
    .Case("res", LVM::RES)
    .Case("rfs", LVM::RFS)
    .Case("rgs", LVM::RGS)
    .Default(-1);
  if (CC != -1)
    return CC;

  return -1;
}

unsigned LVMAsmParser::getReg(int RC, int RegNo) {
  return *(getContext().getRegisterInfo()->getRegClass(RC).begin() + RegNo);
}

int LVMAsmParser::matchRegisterByNumber(unsigned RegNum, StringRef Mnemonic) {
  if (RegNum > 15)
    return -1;

  return getReg(LVM::GR64RegClassID, RegNum);
}

bool LVMAsmParser::tryParseRegisterOperand(OperandVector &Operands,
                                            StringRef Mnemonic) {
  SMLoc S = Parser.getTok().getLoc();
  int RegNo = -1;

  // RegNo = tryParseRegister(Mnemonic);
  if (RegNo == -1)
    return true;

  Operands.push_back(LVMOperand::CreateReg(RegNo, S, Parser.getTok().getLoc()));
  Parser.Lex();   // Eat register token.
  return false;
}

bool LVMAsmParser::ParseOperand(OperandVector &Operands, StringRef Mnemonic) {
  LLVM_DEBUG(dbgs() << "ParseOperand\n");
  OperandMatchResultTy ResTy = MatchOperandParserImpl(Operands, Mnemonic);
  if (ResTy == MatchOperand_Success)
    return false;
  if (ResTy == MatchOperand_ParseFail)
    return true;

  LLVM_DEBUG(dbgs() << ".. Generic Parser\n");

  switch (getLexer().getKind()) {
  default:
    Error(Parser.getTok().getLoc(), "unexpected token in opernad");
    return true;
  case AsmToken::Dollar: {
    SMLoc S = Parser.getTok().getLoc();
    Parser.Lex(); // Eat dollar token.
    // parser register operand
    if (!tryParseRegisterOperand(Operands, Mnemonic)) {
      if (getLexer().is(AsmToken::LParen)) {
        // check if it is indexed addressing operand
        Operands.push_back(LVMOperand::CreateToken("(", S));
        Parser.Lex(); // Eat parenthesis
        if (getLexer().isNot(AsmToken::Dollar))
          return true;

        Parser.Lex(); // Eat dollar
        if (tryParseRegisterOperand(Operands, Mnemonic))
          return true;

        if (!getLexer().is(AsmToken::RParen))
          return true;

        S = Parser.getTok().getLoc();
        Operands.push_back(LVMOperand::CreateToken(")", S));
        Parser.Lex();
      }
      return false;
    }
    // maybe it is a symbol reference
    StringRef Identifier;
    if (Parser.parseIdentifier(Identifier))
      return true;

    SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);

    MCSymbol *Sym = getContext().getOrCreateSymbol("$" + Identifier);

    // Otherwise create a symbol ref.
    const MCExpr *Res = MCSymbolRefExpr::create(Sym, MCSymbolRefExpr::VK_None,
                                                getContext());

    Operands.push_back(LVMOperand::CreateImm(Res, S, E));
    return false;
  }
  case AsmToken::Identifier:
  case AsmToken::LParen:
  case AsmToken::Minus:
  case AsmToken::Plus:
  case AsmToken::Integer:
  case AsmToken::String: {
    const MCExpr *IdVal;
    SMLoc S = Parser.getTok().getLoc();
    if (getParser().parseExpression(IdVal))
      return true;
    SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
    Operands.push_back(LVMOperand::CreateImm(IdVal, S, E));
    return false;
  }
  case AsmToken::Percent: {
    // it is a symbol reference or constant expression
    const MCExpr *IdVal;
    SMLoc S = Parser.getTok().getLoc();
    if (parseRelocOperand(IdVal))
      return true;

    SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);

    Operands.push_back(LVMOperand::CreateImm(IdVal, S, E));
    return false;
  }
  }
  return true;
}

OperandMatchResultTy LVMAsmParser::tryParseRegister(unsigned &RegNo, SMLoc &StartLoc, SMLoc &EndLoc)
{
    SmallVector<std::unique_ptr<MCParsedAsmOperand>, 1> Operands;
    // OperandMatchResultTy ResTy = parseAnyRegister(Operands);
    // if (ResTy == MatchOperand_Success)
    // {
    //     assert(Operands.size() == 1);
    //     LVMOperand &Operand = static_cast<LVMOperand &>(*Operands.front());
    //     StartLoc = Operand.getStartLoc();
    //     EndLoc = Operand.getEndLoc();

    //     // AFAIK, we only support numeric registers and named GPR's in CFI
    //     // directives.
    //     // Don't worry about eating tokens before failing. Using an unrecognised
    //     // register is a parse error.
    //     // if (Operand.isGPRAsmReg()) {
    //     // // Resolve to GPR32 or GPR64 appropriately.
    //     //     RegNo = isGP64bit() ? Operand.getGPR64Reg() : Operand.getGPR32Reg();
    //     // }

    //     return (RegNo == (unsigned)-1) ? MatchOperand_NoMatch : MatchOperand_Success;
    // }

    assert(Operands.size() == 0);
    return (RegNo == (unsigned)-1) ? MatchOperand_NoMatch : MatchOperand_Success;
}

const MCExpr *LVMAsmParser::evaluateRelocExpr(const MCExpr *Expr,
                                               StringRef RelocStr) {
  LVMMCExpr::LVMExprKind Kind =
    StringSwitch<LVMMCExpr::LVMExprKind>(RelocStr)
        .Case("call16", LVMMCExpr::CEK_GOT_CALL)
        .Case("call_hi", LVMMCExpr::CEK_CALL_HI16)
        .Case("call_lo", LVMMCExpr::CEK_CALL_LO16)
        .Case("dtp_hi", LVMMCExpr::CEK_DTP_HI)
        .Case("dtp_lo", LVMMCExpr::CEK_DTP_LO)
        .Case("got", LVMMCExpr::CEK_GOT)
        .Case("got_hi", LVMMCExpr::CEK_GOT_HI16)
        .Case("got_lo", LVMMCExpr::CEK_GOT_LO16)
        .Case("gottprel", LVMMCExpr::CEK_GOTTPREL)
        .Case("gp_rel", LVMMCExpr::CEK_GPREL)
        .Case("hi", LVMMCExpr::CEK_ABS_HI)
        .Case("lo", LVMMCExpr::CEK_ABS_LO)
        .Case("tlsgd", LVMMCExpr::CEK_TLSGD)
        .Case("tlsldm", LVMMCExpr::CEK_TLSLDM)
        .Case("tp_hi", LVMMCExpr::CEK_TP_HI)
        .Case("tp_lo", LVMMCExpr::CEK_TP_LO)
        .Default(LVMMCExpr::CEK_None);

  assert(Kind != LVMMCExpr::CEK_None);
  return LVMMCExpr::create(Kind, Expr, getContext());
}

bool LVMAsmParser::parseRelocOperand(const MCExpr *&Res) {
  Parser.Lex(); // Eat % token
  const AsmToken &Tok = Parser.getTok(); // get next token, operation
  if (Tok.isNot(AsmToken::Identifier))
    return true;

  std::string Str = Tok.getIdentifier().str();

  Parser.Lex(); // Eat identifier
  // now make expression from the rest of the operand
  const MCExpr *IdVal;
  SMLoc EndLoc;

  if (getLexer().getKind() == AsmToken::LParen) {
    while (1) {
      Parser.Lex(); // Eat '(' token
      if (getLexer().getKind() == AsmToken::Percent) {
        Parser.Lex(); // Eat % token
        const AsmToken &NextTok = Parser.getTok();
        if (NextTok.isNot(AsmToken::Identifier))
          return true;
        Str += "(%";
        Str += NextTok.getIdentifier();
        Parser.Lex(); // Eat identifier
        if (getLexer().getKind() != AsmToken::LParen)
          return true;
      } else
        break;
    }
    if (getParser().parseParenExpression(IdVal, EndLoc))
      return true;

    while (getLexer().getKind() == AsmToken::RParen)
      Parser.Lex(); // Eat ")" token
  } else
    return true;

  Res = evaluateRelocExpr(IdVal, Str);
  return false;
}

bool LVMAsmParser::ParseRegister(unsigned &RegNo, SMLoc &StartLoc,
                                   SMLoc &EndLoc) {
  StartLoc = Parser.getTok().getLoc();
  // RegNo = tryParseRegister("");
  EndLoc = Parser.getTok().getLoc();
  return (RegNo == (unsigned)-1);
}

bool LVMAsmParser::parseMemOffset(const MCExpr *&Res) {
  switch(getLexer().getKind()) {
  default:
    return true;
  case AsmToken::Integer:
  case AsmToken::Minus:
  case AsmToken::Plus:
    return (getParser().parseExpression(Res));
  case AsmToken::Percent:
    return parseRelocOperand(Res);
  case AsmToken::LParen:
    return false;
  }
  return true;
}

OperandMatchResultTy LVMAsmParser::parseMemOperand(
    OperandVector &Operands) {
  const MCExpr *IdVal = 0;
  SMLoc S;
  // first operand is the offset
  S = Parser.getTok().getLoc();

  if (parseMemOffset(IdVal))
    return MatchOperand_ParseFail;

  const AsmToken &Tok = Parser.getTok();
  if (Tok.isNot(AsmToken::LParen)) {
    LVMOperand &Mnemonic = static_cast<LVMOperand &>(*Operands[0]);
    if (Mnemonic.getToken() == "la") {
      SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer()-1);
      Operands.push_back(LVMOperand::CreateImm(IdVal, S, E));
      return MatchOperand_Success;
    }
    Error(Parser.getTok().getLoc(), "'(' expected");
    return MatchOperand_ParseFail;
  }

  Parser.Lex();  // Eat '(' token.

  const AsmToken &Tok1 = Parser.getTok();  // get next token
  if (Tok1.is(AsmToken::Dollar)) {
    Parser.Lex();  // Eat '$' token.
    if (tryParseRegisterOperand(Operands, "")) {
      Error(Parser.getTok().getLoc(), "unexpected token in operand");
      return MatchOperand_ParseFail;
    }
  } else {
    Error(Parser.getTok().getLoc(), "unexpected token in operand");
    return MatchOperand_ParseFail;
  }

  const AsmToken &Tok2 = Parser.getTok();  // get next token
  if (Tok2.isNot(AsmToken::RParen)) {
    Error(Parser.getTok().getLoc(), "')' expected");
    return MatchOperand_ParseFail;
  }

  SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);

  Parser.Lex();  // Eat ')' token.

  if (!IdVal)
    IdVal = MCConstantExpr::create(0, getContext());

  // Replace the register operand with the memory operand.
  std::unique_ptr<LVMOperand> op(static_cast<LVMOperand *>(Operands.back().release()));

  int RegNo = op->getReg();
  // remove register from operands
  Operands.pop_back();
  // and add memory operand
  Operands.push_back(LVMOperand::CreateMem(RegNo, IdVal, S, E));
  return MatchOperand_Success;
}

bool LVMAsmParser::parseMathOperation(StringRef Name, SMLoc NameLoc,
                                       OperandVector &Operands) {
  // split the format
  size_t Start = Name.find('.'), Next = Name.rfind('.');
  StringRef Format1 = Name.slice(Start, Next);
  // and add the first format to the operands
  Operands.push_back(LVMOperand::CreateToken(Format1, NameLoc));
  // now for the second format
  StringRef Format2 = Name.slice(Next, StringRef::npos);
  Operands.push_back(LVMOperand::CreateToken(Format2, NameLoc));

  // set the format for the first register
  // setFpFormat(Format1);

  // Read the remaining operands.
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    // Read the first operand.
    if (ParseOperand(Operands, Name)) {
      SMLoc Loc = getLexer().getLoc();
      Parser.eatToEndOfStatement();
      return Error(Loc, "unexpected token in argument list");
    }

    if (getLexer().isNot(AsmToken::Comma)) {
      SMLoc Loc = getLexer().getLoc();
      Parser.eatToEndOfStatement();
      return Error(Loc, "unexpected token in argument list");
    }
    Parser.Lex();  // Eat the comma.

    // Parse and remember the operand.
    if (ParseOperand(Operands, Name)) {
      SMLoc Loc = getLexer().getLoc();
      Parser.eatToEndOfStatement();
      return Error(Loc, "unexpected token in argument list");
    }
  }

  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    SMLoc Loc = getLexer().getLoc();
    Parser.eatToEndOfStatement();
    return Error(Loc, "unexpected token in argument list");
  }

  Parser.Lex(); // Consume the EndOfStatement
  return false;
}

bool LVMAsmParser::
ParseInstruction(ParseInstructionInfo &Info, StringRef Name, SMLoc NameLoc,
                 OperandVector &Operands) {
  // Create the leading tokens for the mnemonic, split by '.' characters.
  size_t Start = 0, Next = Name.find('.');
  StringRef Mnemonic = Name.slice(Start, Next);

  if (Mnemonic == "ret")
    Mnemonic = "jr";

  Operands.push_back(LVMOperand::CreateToken(Mnemonic, NameLoc));

  // Read the remaining operands
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    // Read the first operand
    if (ParseOperand(Operands, Name)) {
      SMLoc Loc = getLexer().getLoc();
      Parser.eatToEndOfStatement();
      return Error(Loc, "unexpected token in argument list");
    }

    while (getLexer().is(AsmToken::Comma)) {
      Parser.Lex();  // Eat the comma

      // Parse and remember the operand
      if (ParseOperand(Operands, Name)) {
        SMLoc Loc = getLexer().getLoc();
        Parser.eatToEndOfStatement();
        return Error(Loc, "unexpected token in argument list");
      }
    }
  }

  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    SMLoc Loc = getLexer().getLoc();
    Parser.eatToEndOfStatement();
    return Error(Loc, "unexpected token in argument list");
  }

  Parser.Lex(); // Consume the EndOfStatement
  return false;
}

bool LVMAsmParser::reportParseError(StringRef ErrorMsg) {
  SMLoc Loc = getLexer().getLoc();
  Parser.eatToEndOfStatement();
  return Error(Loc, ErrorMsg);
}

bool LVMAsmParser::parseSetReorderDirective() {
  Parser.Lex();
  // if this is not the end of the statement, report error
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    reportParseError("unexpected token in statement");
    return false;
  }
  Options.setReorder();
  Parser.Lex();  // Consume the EndOfStatement
  return false;
}

bool LVMAsmParser::parseSetNoReorderDirective() {
  Parser.Lex();
  // if this is not the end of the statement, report error
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    reportParseError("unexpected token in statement");
    return false;
  }
  Options.setNoreorder();
  Parser.Lex();  // Consume the EndOfStatement
  return false;
}

bool LVMAsmParser::parseSetMacroDirective() {
  Parser.Lex();
  // if this is not the end of the statement, report error
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    reportParseError("unexpected token in statement");
    return false;
  }
  Options.setMacro();
  Parser.Lex();  // Consume the EndOfStatement
  return false;
}

bool LVMAsmParser::parseSetNoMacroDirective() {
  Parser.Lex();
  // if this is not the end of the statement, report error
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    reportParseError("noreorder must be set before nomacro");
    return false;
  }
  if (Options.isReorder()) {
    reportParseError("noreorder must be set before nomacro");
    return false;
  }
  Options.setNomacro();
  Parser.Lex();  // Consume the EndOfStatement
  return false;
}

bool LVMAsmParser::parseDirectiveSet() {
  //get next token
  const AsmToken &Tok = Parser.getTok();

  if (Tok.getString() == "reorder") {
    return parseSetReorderDirective();
  } else if (Tok.getString() == "noreorder") {
    return parseSetNoReorderDirective();
  } else if (Tok.getString() == "macro") {
    return parseSetMacroDirective();
  } else if (Tok.getString() == "nomacro") {
    return parseSetNoMacroDirective();
  }
  return true;
}

bool LVMAsmParser::ParseDirective(AsmToken DirectiveID) {
  if (DirectiveID.getString() == ".ent") {
    // ignore this directive for now
    Parser.Lex();
    return false;
  }

  if (DirectiveID.getString() == ".end") {
    // ignore this directive for now
    Parser.Lex();
    return false;
  }

  if (DirectiveID.getString() == ".frame") {
    // ignore this directive for now
    Parser.eatToEndOfStatement();
    return false;
  }

  if (DirectiveID.getString() == ".set") {
    return parseDirectiveSet();
  }

  if (DirectiveID.getString() == ".fmask") {
    // ignore this directive for now
    Parser.eatToEndOfStatement();
    return false;
  }

  if (DirectiveID.getString() == ".mask") {
    // ignore this directive for now
    Parser.eatToEndOfStatement();
    return false;
  }

  if (DirectiveID.getString() == ".gpword") {
    // ignore this directive for now
    Parser.eatToEndOfStatement();
    return false;
  }

  return true;
}

extern "C" void LLVMInitializeLVMAsmParser() {
    RegisterMCAsmParser<LVMAsmParser> X(getLVMTarget());
}

#define GET_REGISTER_MATCHER
#define GET_MATCHER_IMPLEMENTATION
#include "LVMGenAsmMatcher.inc"

