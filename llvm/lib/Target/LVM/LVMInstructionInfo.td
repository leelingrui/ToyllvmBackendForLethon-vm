class I32<bits<6> Opcod, bits<2> Optype, dag outs, dag ins, string AsmStr, list<dag> pattern> : Instruction 
{
    let Size = 4;
    let Namespace = "LVM";
    bits<32> Inst;
    let Inst{5-0} = Opcod;
    let Inst{7-6} = Optype;
    dag OutOperandList = outs;
    dag InOperandList = ins;
    string AsmString = AsmStr;
    let Pattern = pattern;
    bits<8> rs2;
    bits<8> rs1;
    bits<8> rd;
    let Inst{15-8} = rd;
    let Inst{23-16} = rs1;
    let Inst{31-24} = rs2;
}

class I24<bits<6> Opcod, bits<2> Optype, dag outs, dag ins, string AsmStr, list<dag> pattern> : Instruction 
{
    let Size = 3;
    let Namespace = "LVM";
    bits<24> Inst;
    let Inst{5-0} = Opcod;
    let Inst{7-6} = Optype;
    dag OutOperandList = outs;
    dag InOperandList = ins;
    string AsmString = AsmStr;
    let Pattern = pattern;
    bits<8> rs;
    bits<8> rd;
    let Inst{15-8} = rd;
    let Inst{23-16} = rs;
}

class I16<bits<6> Opcod, bits<2> Optype, dag outs, dag ins, string AsmStr, list<dag> pattern> : Instruction 
{
    let Size = 2;
    let Namespace = "LVM";
    bits<16> Inst;
    let Inst{5-0} = Opcod;
    let Inst{7-6} = Optype;
    dag OutOperandList = outs;
    dag InOperandList = ins;
    string AsmString = AsmStr;
    let Pattern = pattern;
    bits<8> rd;
    let Inst{15-8} = rd;
}

class I8<bits<6> Opcod, bits<2> Optype, string AsmStr, list<dag> pattern> : Instruction
{
    let Size = 1;
    let Namespace = "LVM";
    bits<8> Inst;
    let Inst{5-0} = Opcod;
    let Inst{7-6} = Optype;
    let OutOperandList = (outs);
    let InOperandList = (ins);
    string AsmString = AsmStr;
    let Pattern = pattern;
}

class I<dag outs, dag ins, string AsmStr, list<dag> pattern> : Instruction
{
    let Namespace = "LVM";
    let OutOperandList = outs;
    let InOperandList = ins;
    string AsmString = AsmStr;
    let Pattern = pattern;
}


// class WrapperPat<SDNode node, Instruction ORiOp, RegisterClass RC> : Pat<(LVMWrapper RC:$gp, node:$in), (ORiOp RC:$gp, node:$in)>;
// def : WrapperPat<tglobaladdr, ORi, GR64>;

// def : WrapperPat<tjumptable, ORi, GR64>;

def LVMMemAsmOperand : AsmOperandClass {
    let Name = "Mem";
    let ParserMethod = "parseMemOperand";
}

def mem : Operand<iPTR> {
    let PrintMethod = "printMemOperand";
    let MIOperandInfo = (ops GR64, GR64);
    let EncoderMethod = "getMemEncoding";
    let ParserMatchClass = LVMMemAsmOperand;
}

def SDT_LVMCallSeqStart : SDCallSeqStart<[SDTCisVT<0, i64>, SDTCisVT<1, i64>]>;
def SDT_LVMCallSeqEnd : SDCallSeqEnd<[SDTCisVT<0, i64>, SDTCisVT<1, i64>]>;
def SDT_LVMCall : SDTypeProfile<0, 1, [SDTCisVT<0, iPTR>]>;
def LVMTailCall : SDNode<"LVMISD::TAILCALL", SDT_LVMCall,
                          [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

def LVMHi    : SDNode<"LVMISD::Hi", SDTIntUnaryOp>;
def LVMLo    : SDNode<"LVMISD::Lo", SDTIntUnaryOp>;
def LVMRetFlag : SDNode<"LVMISD::RET_FLAG", SDTNone, [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
// def LVMAddFlag : SDNode<"LVMISD::ADD_FLAG", SDTNone, [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
def LVMCall : SDNode<"LVMISD::CALL", SDT_LVMCall,
                         [SDNPHasChain, SDNPOutGlue, SDNPOptInGlue,
                          SDNPVariadic]>;
def LVMWrapper : SDNode<"LVMISD::Wrapper", SDTIntBinOp>;

def callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_LVMCallSeqStart,
                           [SDNPHasChain, SDNPSideEffect, SDNPOutGlue]>;
def callseq_end : SDNode<"ISD::CALLSEQ_END", SDT_LVMCallSeqEnd,
                         [SDNPHasChain, SDNPOptInGlue,
                          SDNPSideEffect, SDNPOutGlue]>;

class ImmSExtAsmOperandClass : AsmOperandClass {
  let SuperClasses = [ImmAsmOperand];
  let RenderMethod = "addImmOperands";
}

// [0, 0x0000007F] | [0x000000000000FF80, 0x000000000000FFFF] |
//   [0xFFFFFFFFFFFFFF80, 0xFFFFFFFFFFFFFFFF]
def ImmSExti16i8AsmOperand : ImmSExtAsmOperandClass {
  let Name = "ImmSExti16i8";
}

// [0, 0x0000007F] | [0x00000000FFFFFF80, 0x00000000FFFFFFFF] |
//   [0xFFFFFFFFFFFFFF80, 0xFFFFFFFFFFFFFFFF]
def ImmSExti32i8AsmOperand : ImmSExtAsmOperandClass {
  let Name = "ImmSExti32i8";
}

// [0, 0x0000007F]                                            |
//   [0xFFFFFFFFFFFFFF80, 0xFFFFFFFFFFFFFFFF]
def ImmSExti64i8AsmOperand : ImmSExtAsmOperandClass {
  let Name = "ImmSExti64i8";
}


def RelocPIC        : Predicate<"TM.getRelocationModel() == Reloc::PIC_">;

def Imm8 : Operand<i8>, ImmLeaf<i8, [{return isInt<8>(Imm);}]>;
def Imm16 : Operand<i16>, ImmLeaf<i16, [{return isInt<16>(Imm);}]>;
def Imm32 : Operand<i32>, ImmLeaf<i32, [{return isInt<32>(Imm);}]>;
def Imm64 : Operand<i64>, ImmLeaf<i64, [{return isInt<64>(Imm);}]>;

def i16immSExt8 : ImmLeaf<i16, [{ return isInt<8>(Imm); }]>;
def i32immSExt8 : ImmLeaf<i32, [{ return isInt<8>(Imm); }]>;
def i64immSExt8 : ImmLeaf<i64, [{ return isInt<8>(Imm); }]>;

def jmptarget : Operand<OtherVT> {
    let EncoderMethod = "getJumpTargetOpValue";
    let OperandType = "OPERAND_PCREL";
}

def i16i8imm  : Operand<i16> {
    let ParserMatchClass = ImmSExti16i8AsmOperand;
    let OperandType = "OPERAND_IMMEDIATE";
}
def i32i8imm  : Operand<i32> {
    let ParserMatchClass = ImmSExti32i8AsmOperand;
    let OperandType = "OPERAND_IMMEDIATE";
}
def i64i8imm  : Operand<i64> {
    let ParserMatchClass = ImmSExti64i8AsmOperand;
    let OperandType = "OPERAND_IMMEDIATE";
}

class AlignedLoad<PatFrag Node> : PatFrag<(ops node:$ptr), (Node node:$ptr),
            [{
              LoadSDNode *LD = cast<LoadSDNode>(N);
              return LD->getMemoryVT().getSizeInBits() / 8 <= LD->getAlignment();
            }]>;

class AlignedStore<PatFrag Node>
  : PatFrag<(ops node:$val, node:$ptr), (Node node:$val, node:$ptr),
            [{
              StoreSDNode *ST = cast<StoreSDNode>(N);
              return ST->getMemoryVT().getSizeInBits() / 8 <= ST->getAlignment();
            }]>;

def addr : ComplexPattern<iPTR, 2, "SelectAddr",
                          [frameindex], [SDNPWantParent]>;


def addr_i : ComplexPattern<iPTR, 2, "SelectAddr_I",
                          [frameindex], [SDNPWantParent]>;

def store_a : AlignedStore<store>;
def load_a : AlignedLoad<load>;

class LVMPseudo<dag outs, dag ins, string asmstr, list<dag> pattern> : I<outs, ins, asmstr, pattern> {
    let isPseudo = 1;
    let isCodeGenOnly = 1;
}

let Defs = [RSP, RBP, RFLAGS], Uses = [RSP, RBP], hasSideEffects = 1, hasNoSchedulingInfo = 1 in {
    // As stack alignment is always done with addiu, we need a 16-bit immediate
    def ADJCALLSTACKDOWN : LVMPseudo<(outs), (ins i64imm:$amt1, i64imm:$amt2), "#ADJCALLSTACKDOWN",
                                    [(callseq_end timm:$amt1, timm:$amt2)]>
    {
        // let EncodingInfos = EncodingByHwMode;
    }
    def ADJCALLSTACKUP : LVMPseudo<(outs), (ins i64imm:$amt1, i64imm:$amt2), "#ADJCALLSTACKUP",
                                  [(callseq_start timm:$amt1, timm:$amt2)]>
    {
        // let EncodingInfos = EncodingByHwMode;
    }
}

def RET : I8<0b011111, 0b00, "ret", [(LVMRetFlag)]>
{
    let isReturn = 1;
    let isTerminator = 1;
}

multiclass Load
{
    def _8M : I32<0b001001, 0b00, (outs GR8:$rd), (ins mem:$rs), "mov\t$rd, $rs", [(set GR8:$rd, (load_a addr:$rs))]>
    {
        let Inst{9-8} = 0b11;
        let Inst{15-10} = rd{5-0};
    }
    def _8I : I24<0b001001, 0b00, (outs GR8:$rd), (ins Imm8:$rs), "mov\t$rd, $rs", [(set GR8:$rd, Imm8:$rs)]>
    {
        let Inst{9-8} = 0b00;
        let Inst{15-10} = rd{5-0};
    }
    def _16M : I32<0b001001, 0b01, (outs GR16:$rd), (ins mem:$rs), "mov\t$rd, $rs", [(set GR16:$rd, (load_a addr:$rs))]>
    {
        let Inst{9-8} = 0b11;
        let Inst{15-10} = rd{5-0};
    }
    def _16I : I24<0b001001, 0b01, (outs GR16:$rd), (ins i16i8imm:$rs), "mov\t$rd, $rs", [(set GR16:$rd, i16immSExt8:$rs)]>
    {
        let Inst{9-8} = 0b00;
        let Inst{15-10} = rd{5-0};
    }
    def _32M : I32<0b001001, 0b10, (outs GR32:$rd), (ins mem:$rs), "mov\t$rd, $rs", [(set GR32:$rd, (load_a addr:$rs))]>
    {
        let Inst{9-8} = 0b11;
        let Inst{15-10} = rd{5-0};
    }
    def _32I : I24<0b001001, 0b10, (outs GR32:$rd), (ins i32i8imm:$rs), "mov\t$rd, $rs", [(set GR32:$rd, i32immSExt8:$rs)]>
    {
        let Inst{9-8} = 0b00;
        let Inst{15-10} = rd{5-0};
    }
    def _64M : I32<0b001001, 0b11, (outs GR64:$rd), (ins mem:$rs), "mov\t$rd, $rs", [(set GR64:$rd, (load_a addr:$rs))]>
    {
        let Inst{9-8} = 0b11;
        let Inst{15-10} = rd{5-0};
    }
    def _64I : I24<0b001001, 0b11, (outs GR64:$rd), (ins i64i8imm:$rs), "mov\t$rd, $rs", [(set GR64:$rd, i64immSExt8:$rs)]>
    {
        let Inst{9-8} = 0b00;
        let Inst{15-10} = rd{5-0};
    }
}

multiclass Store
{
    def _8IR : I32<0b001010, 0b00, (outs), (ins mem:$rd1, GR8:$rs1), "mov\t$rd1, $rs1", [(store_a GR8:$rs1, addr_i:$rd1)]>
    {
        bits<16> rd1;
        let Inst{9-8} = 0b10;
        let Inst{15-10} = rd1{5-0};
        let Inst{23-16} = rd1{15-8};
        let Inst{31-24} = rs1{7-0};
    }
    def _8RR : I32<0b001010, 0b00, (outs), (ins mem:$rd1, GR8:$rs1), "mov\t$rd1, $rs1", [(store_a GR8:$rs1, addr:$rd1)]>
    {
        bits<16> rd1;
        let Inst{9-8} = 0b11;
        let Inst{15-10} = rd1{5-0};
        let Inst{23-16} = rd1{15-8};
        let Inst{31-24} = rs1{7-0};
    }
    def _16IR : I32<0b001010, 0b01, (outs), (ins mem:$rd1, GR16:$rs1), "mov\t$rd1, $rs1", [(store_a GR16:$rs1, addr_i:$rd1)]>
    {
        bits<16> rd1;
        let Inst{9-8} = 0b10;
        let Inst{15-10} = rd1{5-0};
        let Inst{23-16} = rd1{15-8};
        let Inst{31-24} = rs1{7-0};
    }
    def _16RR : I32<0b001010, 0b01, (outs), (ins mem:$rd1, GR16:$rs1), "mov\t$rd1, $rs1", [(store_a GR16:$rs1, addr:$rd1)]>
    {
        bits<16> rd1;
        let Inst{9-8} = 0b11;
        let Inst{15-10} = rd1{5-0};
        let Inst{23-16} = rd1{15-8};
        let Inst{31-24} = rs1{7-0};
    }
    def _32IR : I32<0b001010, 0b10, (outs), (ins mem:$rd1, GR32:$rs1), "mov\t$rd1, $rs1", [(store_a GR32:$rs1, addr_i:$rd1)]>
    {
        bits<16> rd1;
        let Inst{9-8} = 0b10;
        let Inst{15-10} = rd1{5-0};
        let Inst{23-16} = rd1{15-8};
        let Inst{31-24} = rs1{7-0};
    }
    def _32RR : I32<0b001010, 0b10, (outs), (ins mem:$rd1, GR32:$rs1), "mov\t$rd1, $rs1", [(store_a GR32:$rs1, addr:$rd1)]>
    {
        bits<16> rd1;
        let Inst{9-8} = 0b11;
        let Inst{15-10} = rd1{5-0};
        let Inst{23-16} = rd1{15-8};
        let Inst{31-24} = rs1{7-0};
    }
    def _64IR : I32<0b001010, 0b11, (outs), (ins mem:$rd1, GR64:$rs1), "mov\t$rd1, $rs1", [(store_a GR64:$rs1, addr_i:$rd1)]>
    {
        bits<16> rd1;
        let Inst{9-8} = 0b10;
        let Inst{15-10} = rd1{5-0};
        let Inst{23-16} = rd1{15-8};
        let Inst{31-24} = rs1{7-0};
    }
    def _64RR : I32<0b001010, 0b11, (outs), (ins mem:$rd1, GR64:$rs1), "mov\t$rd1, $rs1", [(store_a GR64:$rs1, addr:$rd1)]>
    {
        bits<16> rd1;
        let Inst{9-8} = 0b11;
        let Inst{15-10} = rd1{5-0};
        let Inst{23-16} = rd1{15-8};
        let Inst{31-24} = rs1{7-0};
    }
}

multiclass Mov
{
    def _8rr : I24<0b100001, 0b00, (outs GR8:$rd), (ins GR8:$rs), "mov\t$rd, $rs", []>;
    def _16rr : I24<0b100001, 0b01, (outs GR16:$rd), (ins GR16:$rs), "mov\t$rd, $rs", []>;
    def _32rr : I24<0b100001, 0b10, (outs GR32:$rd), (ins GR32:$rs), "mov\t$rd, $rs", []>;
    def _64rr : I24<0b100001, 0b11, (outs GR64:$rd), (ins GR64:$rs), "mov\t$rd, $rs", []>;
}

multiclass Push
{
    def _8R : I16<0b000111, 0b00, (outs), (ins GR8:$rs), "push\t$rs", []>;
    def _16R : I16<0b000111, 0b01, (outs), (ins GR16:$rs), "push\t$rs", []>;
    def _32R : I16<0b000111, 0b10, (outs), (ins GR32:$rs), "push\t$rs", []>;
    def _64R : I16<0b000111, 0b11, (outs), (ins GR64:$rs), "push\t$rs", []>;
}

multiclass Pop
{
    def _8R : I16<0b001000, 0b00, (outs GR8:$rd), (ins), "pop\t$rd", []>;
    def _16R : I16<0b001000, 0b01, (outs GR16:$rd), (ins), "pop\t$rd", []>;
    def _32R : I16<0b001000, 0b10, (outs GR32:$rd), (ins), "pop\t$rd", []>;
    def _64R : I16<0b001000, 0b11, (outs GR64:$rd), (ins), "pop\t$rd", []>;
}

def calltarget : Operand<iPTR>
{
    let EncoderMethod = "getJumpTargetOpValue";
}


multiclass MathOpBin<bits<6> Opcod, string AsmStr, SDNode opNode>
{
    def _8rr : I32<Opcod, 0b00, (outs GR8:$rd), (ins GR8:$rs1, GR8:$rs2), !strconcat(AsmStr, "\t$rd, $rs1, $rs2"), [(set GR8:$rd, (opNode GR8:$rs1, GR8:$rs2))]>;
    def _16rr : I32<Opcod, 0b01, (outs GR16:$rd), (ins GR16:$rs1, GR16:$rs2), !strconcat(AsmStr, "\t$rd, $rs1, $rs2"), [(set GR16:$rd, (opNode GR16:$rs1, GR16:$rs2))]>;
    def _32rr : I32<Opcod, 0b10, (outs GR32:$rd), (ins GR32:$rs1, GR32:$rs2), !strconcat(AsmStr, "\t$rd, $rs1, $rs2"), [(set GR32:$rd, (opNode GR32:$rs1, GR32:$rs2))]>;
    def _64rr : I32<Opcod, 0b11, (outs GR64:$rd), (ins GR64:$rs1, GR64:$rs2), !strconcat(AsmStr, "\t$rd, $rs1, $rs2"), [(set GR64:$rd, (opNode GR64:$rs1, GR64:$rs2))]>;
}

// let isCall = 1, isTerminator = 1, isReturn = 1, isBarrier = 1, hasDelaySlot = 1,
//     hasExtraSrcRegAllocReq = 1, Defs = [AT] in {
//   class TailCall<Instruction JumpInst> :
//     PseudoSE<(outs), (ins calltarget:$target), [], IIBranch>,
//     PseudoInstExpansion<(JumpInst jmptarget:$target)>;

//   class TailCallReg<RegisterClass RO, Instruction JRInst,
//                     RegisterClass ResRO = RO> :
//     PseudoSE<(outs), (ins RO:$rs), [(Cpu0TailCall RO:$rs)], IIBranch>,
//     PseudoInstExpansion<(JRInst ResRO:$rs)>;
// }
class CallReg<bits<6> Opcod, bits<2> Sel, string instrAsm> : I16<Opcod, Sel, (outs), (ins GR64:$rd, variable_ops),  !strconcat(instrAsm, "\t$rd"), [(LVMCall GR64:$rd)]>
{
    let DecoderMethod = "DecodeJumpTarget";
    let isCall = 1;
}
class Call<bits<6> Opcod, bits<2> Sel, string instrAsm> : I<(outs), (ins calltarget:$rd, variable_ops),  !strconcat(instrAsm, "\t$rd"), [(LVMCall iPTR:$rd)]>
{
    let Size = 9;
    bits<72> Inst;
    let Inst{5-0} = Opcod;
    let Inst{7-6} = Sel;
    bits<64> dst;
    let Inst{71-8} = dst;
    let isCall = 1;
}

multiclass ConditionBranch<bits<6> Opcod, bits<2> Sel, string instrAsm, SDNode opNode>
{
    let isBranch = 1, isTerminator = 1 in
    {
        def _I : I32<Opcod, Sel, (outs), (ins jmptarget:$addr), !strconcat(instrAsm, "\t$addr"), []>
        {
            bits<8> cond;
            bits<16> addr;
            let cond = 0;
            let Inst{15-8} = cond;
            let Inst{31-16} = addr;
        }
        // def _R : I24<Opcod, Sel, (outs), (ins GR64:$rs), !strconcat(instrAsm, "\t$rs"), [(opNode GR64:$rs)]>
        // {
        //     let rd = 0;
        // }
    }
}

defm JMP : ConditionBranch<0b001101, 0, "JMP", br>;
def CALLI : Call<0b011110, 0, "call">;
def CALLR : CallReg<0b011110, 1, "call">;
defm MOV : Mov;
defm POP : Pop;
defm PUSH : Push;
defm STR : Store;
defm LDR : Load;
defm ADD : MathOpBin<0b000001, "add", add>;
// defm SHL : MathOpBin<0b100011, "shl", shl>;
// defm SHR : MathOpBin<0b100100, "shr", srl>;
// defm SAR : MathOpBin<0b100101, "sar", sra>;
// defm ROL : MathOpBin<0b100111, "rol", rotl>;
// defm ROR : MathOpBin<0b101000, "ror", rotr>;
//defm ADDI : MathOpBin<0b000010, "addi", addi>;
defm SUB : MathOpBin<0b000011, "sub", sub>;
//defm SUBI : MathOpBin<0b000100, "subi", subi>;

// def : Pat<(set GR64:$rd, iPTR:$rs), (LDR_64I, iPTR:$rs)>;