#include "LVMISelLowering.h"
#include "MCTargetDesc/LVMMCTargetDesc.h"
#include "LVMSubtarget.h"
#include "llvm/ADT/Statistic.h"
#include "LVMTargetMachine.h"
#include "LVMMachineFunctionInfo.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineRegionInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/Register.h"
#include "llvm/Support/Debug.h"
using namespace llvm;
#include "LVMGenCallingConv.inc"

#define DEBUG_TYPE "LVM-lower"
STATISTIC(NumTailCalls, "Number of tail calls");
static cl::opt<bool> EnableLVMTailCalls("enable-LVM-tail-calls", cl::Hidden,
                    cl::desc("LVM: Enable tail calls."),
                    cl::init(false));

LVMTargetLowering::LVMCC::SpecialCallingConvType LVMTargetLowering::getSpecialCallingConv(SDValue Callee) const
{
    LVMCC::SpecialCallingConvType SpecialCallConv = LVMCC::NoSpecialCallingConv;
    return SpecialCallConv;
}

bool llvm::LVMTargetLowering::isEligibleForTailCallOptimization(const LVMCC &LVMCCInfo, unsigned NextStackOffset, const LVMMachineFunctionInfo &FI) const {
    if (!EnableLVMTailCalls)
        return false;

    // Return false if either the callee or caller has a byval argument.
    if (LVMCCInfo.hasByValArg() || FI.hasByvalArg())
        return false;

    // Return true if the callee's argument area is no larger than the
    // calleer's.
    return NextStackOffset <= FI.getIncomingArgSize();
}

SDValue llvm::LVMTargetLowering::LowerCall(TargetLowering::CallLoweringInfo &CLI,
                                   SmallVectorImpl<SDValue> &InVals) const
{
     SelectionDAG &DAG = CLI.DAG;
    SDLoc DL = CLI.DL;
    SmallVectorImpl<ISD::OutputArg> &Outs = CLI.Outs;
    SmallVectorImpl<SDValue> &OutVals = CLI.OutVals;
    SmallVectorImpl<ISD::InputArg> &Ins = CLI.Ins;
    SDValue Chain = CLI.Chain;
    SDValue Callee = CLI.Callee;
    bool &IsTailCall = CLI.IsTailCall;
    CallingConv::ID CallConv = CLI.CallConv;
    bool IsVarArg = CLI.IsVarArg;

    MachineFunction &MF = DAG.getMachineFunction();
    MachineFrameInfo &MFI = MF.getFrameInfo();
    const TargetFrameLowering *TFL = MF.getSubtarget().getFrameLowering();
    LVMMachineFunctionInfo *FuncInfo = MF.getInfo<LVMMachineFunctionInfo>();
    bool IsPIC = isPositionIndependent();

    // Analyze operands of the call, assigning locations to each operand.
    SmallVector<CCValAssign, 16> ArgLocs;
    CCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), ArgLocs,
                    *DAG.getContext());
    LVMCC::SpecialCallingConvType SpecialCallingConv =
        getSpecialCallingConv(Callee);
    LVMCC LVMCCInfo(CallConv, CCInfo, SpecialCallingConv);

    LVMCCInfo.analyzeCallOperands(Outs, IsVarArg, Subtarget.abiUsesSoftFloat(),
                                    Callee.getNode(), CLI.getArgs());

    // Get a count of how many bytes are to be pushed on the stack.
    unsigned NextStackOffset = CCInfo.getNextStackOffset();

    // Check if it's really possible to do a tail call.
    if (IsTailCall)
        IsTailCall = isEligibleForTailCallOptimization(
            LVMCCInfo, NextStackOffset, *MF.getInfo<LVMMachineFunctionInfo>());
    if (!IsTailCall && CLI.CB && CLI.CB->isMustTailCall())
        report_fatal_error("failed to perform tail call elimination on a call "
                        "site marked musttail");

    if (IsTailCall)
        ++NumTailCalls;

    // Chain is the output chain of the last Load/Store or CopyToReg node.
    // ByValChain is the output chain of the last Memcpy node created for copying
    // byval arguments to the stack.
    unsigned StackAlignment = TFL->getStackAlignment();
    NextStackOffset = alignTo(NextStackOffset, StackAlignment);
    SDValue NextStackOffsetVal = DAG.getIntPtrConstant(NextStackOffset, DL, true);

    if (!IsTailCall)
        Chain = DAG.getCALLSEQ_START(Chain, NextStackOffset, 0, DL);

    SDValue StackPtr = DAG.getCopyFromReg(Chain, DL, LVM::RSP, getPointerTy(DAG.getDataLayout()));

    // With EABI is it possible to have 16 args on registers.
    std::deque<std::pair<unsigned, SDValue>> RegsToPass;
    SmallVector<SDValue, 8> MemOpChains;
    LVMCC::byval_iterator ByValArg = LVMCCInfo.byval_begin();

    // Walk the register/memloc assignments, inserting copies/loads.
    for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
        SDValue Arg = OutVals[i];
        CCValAssign &VA = ArgLocs[i];
        MVT LocVT = VA.getLocVT();
        ISD::ArgFlagsTy Flags = Outs[i].Flags;

    if (Flags.isByVal()) {
      assert(Flags.getByValSize() &&
             "ByVal args of size 0 should have been ignored by front-end.");
      assert(ByValArg != LVMCCInfo.byval_end());
      assert(!IsTailCall &&
             "Do not tail-call optimize if there is a byval argument.");
      passByValArg(Chain, DL, RegsToPass, MemOpChains, StackPtr, MFI, DAG, Arg,
                   LVMCCInfo, *ByValArg, Flags, Subtarget.isLittle());
      ++ByValArg;
      continue;  
    }

    switch (VA.getLocInfo()) {
    default: llvm_unreachable("Unknown loc info!");
    case CCValAssign::Full:
        break;
    case CCValAssign::SExt:
        Arg = DAG.getNode(ISD::SIGN_EXTEND, DL, LocVT, Arg);
        break;
    case CCValAssign::ZExt:
        Arg = DAG.getNode(ISD::ZERO_EXTEND, DL, LocVT, Arg);
        break;
    case CCValAssign::AExt:
        Arg = DAG.getNode(ISD::ANY_EXTEND, DL, LocVT, Arg);
        break;
    }

    // Arguments that can be passed on register must be kept at
    // RegsToPass vector.
    if (VA.isRegLoc()) {
      RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
      continue;
    }

    // Register can't get to this point...
    assert(VA.isMemLoc());

    // emit ISD::STORE whichs stores the parameter value to a stack location.
    MemOpChains.push_back(passArgOnStack(StackPtr, VA.getLocMemOffset(),
                                         Chain, Arg, DL, IsTailCall, DAG));
    }

    // Transform all store nodes into one single node because all store
    // nodes are independent of each other.
    if (!MemOpChains.empty())
        Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other, MemOpChains);

    // If the callee is a GlobalAddress/ExternalSymbol node (quite common, every
    // direct call is) turn it into a TargetGlobalAddress/TargetExternalSymbol
    // node so that legalize doesn't hack it.
    bool IsPICCall = IsPIC; // true if calls are translated to jalr $t9

    bool GlobalOrExternal = false, InternalLinkage = false;
    SDValue CalleeLo;
    EVT Ty = Callee.getValueType();

    if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee)) {
        if (IsPICCall)
        {
            const GlobalValue *Val = G->getGlobal();
            InternalLinkage = Val->hasInternalLinkage();

            if (InternalLinkage)
                Callee = getAddrLocal(G, Ty, DAG);
            else
                Callee = getAddrGlobal(G, Ty, DAG, LVMII::MO_GOT_CALL, Chain,
                                    FuncInfo->callPtrInfo(Val));
        }
        else
            Callee = DAG.getTargetGlobalAddress(G->getGlobal(), DL,
                                            getPointerTy(DAG.getDataLayout()), 0,
                                            LVMII::MO_NO_FLAG);
        GlobalOrExternal = true;
    } else if (ExternalSymbolSDNode *S = dyn_cast<ExternalSymbolSDNode>(Callee)) {
        const char *Sym = S->getSymbol();

        if (!IsPIC) // static
        Callee = DAG.getTargetExternalSymbol(Sym, getPointerTy(DAG.getDataLayout()),
                                            LVMII::MO_NO_FLAG);
        else // PIC
        Callee = getAddrGlobal(S, Ty, DAG, LVMII::MO_GOT_CALL, Chain,
                                FuncInfo->callPtrInfo(Sym));

        GlobalOrExternal = true;
    }

    SmallVector<SDValue, 8> Ops(1, Chain);
    SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
    getOpndList(Ops, RegsToPass, IsPICCall, GlobalOrExternal, InternalLinkage,
                CLI, Callee, Chain);

    if (IsTailCall)
    {
        return DAG.getNode(LVMISD::TAILCALL, DL, MVT::Other, Ops);
    }
    else
    {
        Chain = DAG.getNode(LVMISD::CALL, DL, NodeTys, Ops);
    }
    SDValue InFlag = Chain.getValue(1);

    // Create the CALLSEQ_END node.
    Chain = DAG.getCALLSEQ_END(Chain, NextStackOffsetVal,
                                DAG.getIntPtrConstant(0, DL, true), InFlag, DL);
    InFlag = Chain.getValue(1);

    // Handle result values, copying them out of physregs into vregs that we
    // return.
    return LowerCallResult(Chain, InFlag, CallConv, IsVarArg,
                            Ins, DL, DAG, InVals, CLI.Callee.getNode(), CLI.RetTy);
}

SDValue llvm::LVMTargetLowering::LowerCallResult(SDValue Chain, SDValue InFlag, CallingConv::ID CallConv, bool isVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL, SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals, const SDNode *CallNode,
    const Type *RetTy) const
    {
        // Assign location to each value returned by this call.
        SmallVector<CCValAssign, 16> RVLocs;
        CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(),
                        RVLocs, *DAG.getContext());

        LVMCC LVMCCInfo(CallConv, CCInfo);

        LVMCCInfo.analyzeCallResult(Ins, Subtarget.abiUsesSoftFloat(),
                                    CallNode, RetTy);

        // Copy all of the result registers out of their specified physreg.
        for (unsigned i = 0; i != RVLocs.size(); ++i) {
            SDValue Val = DAG.getCopyFromReg(Chain, DL, RVLocs[i].getLocReg(),
                                            RVLocs[i].getLocVT(), InFlag);
            Chain = Val.getValue(1);
            InFlag = Val.getValue(2);

            if (RVLocs[i].getValVT() != RVLocs[i].getLocVT())
            Val = DAG.getNode(ISD::BITCAST, DL, RVLocs[i].getValVT(), Val);

            InVals.push_back(Val);
        }

        return Chain;
}

LVMTargetLowering::LVMTargetLowering(LVMTargetMachine &TM)
    : TargetLowering(TM), Subtarget(*TM.getSubtargetImpl()) {
  addRegisterClass(MVT::i64, &LVM::GR64RegClass);
  addRegisterClass(MVT::i32, &LVM::GR32RegClass);
  addRegisterClass(MVT::i16, &LVM::GR16RegClass);
  addRegisterClass(MVT::i8, &LVM::GR8RegClass);
  computeRegisterProperties(Subtarget.getRegisterInfo());
}
static const MCPhysReg Regs64[] = { LVM::RCX, LVM::RDX };
const char *LVMTargetLowering::getTargetNodeName(unsigned Opcode) const
{
    switch (Opcode) {
    case LVMISD::RET_FLAG:
        return "LVM::RET_FLAG";
    case LVMISD::JMPLINK:
        return "LVM::JMPLINK";
    case LVMISD::TAILCALL:
        return "LVM::TAILCALL";
    case LVMISD::CALL:
        return "LVM::CALL";
    default:
        return nullptr;
    }
}

static unsigned addLiveIn(MachineFunction &MF, unsigned PReg, const TargetRegisterClass *RC) {
  unsigned VReg = MF.getRegInfo().createVirtualRegister(RC);
  MF.getRegInfo().addLiveIn(PReg, VReg);
  return VReg;
}

SDValue LVMTargetLowering::LowerFormalArguments(SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
        const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL, SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const 
{
    SmallVector<CCValAssign, 16> ArgLocs;
    std::vector<SDValue> OutChains;
    Function::const_arg_iterator FuncArg = DAG.getMachineFunction().getFunction().arg_begin();
    MachineFunction &MF = DAG.getMachineFunction();
    MachineFrameInfo &MFI = MF.getFrameInfo();
    LVMMachineFunctionInfo *LVMFI = MF.getInfo<LVMMachineFunctionInfo>();
    CCState CCInfo(CallConv, IsVarArg, MF, ArgLocs, *DAG.getContext());
    CCInfo.AnalyzeFormalArguments(Ins, CC_LVM);
    LVMCC LVMCCInfo(CallConv, CCInfo);
    LVMCC::byval_iterator ByValArg = LVMCCInfo.byval_begin();
    size_t CurArgIdx = 0;
    for (unsigned i = 0, e = ArgLocs.size(); i < e; ++i) {
        CCValAssign &VA = ArgLocs[i];
        if (Ins[i].isOrigArg())
        {
            std::advance(FuncArg, Ins[i].getOrigArgIndex() - CurArgIdx);
            CurArgIdx = Ins[i].getOrigArgIndex();
        }
        EVT ValVT = VA.getValVT();
        ISD::ArgFlagsTy Flags = Ins[i].Flags;
        bool IsRegLoc = VA.isRegLoc();
        if (Flags.isByVal())
        {
            assert(Flags.getByValSize() && "ByVal args of size 0 should have been ignored by front-end.");
            assert(ByValArg != LVMCCInfo.byval_end());
            copyByValRegs(Chain, DL, OutChains, DAG, Flags, InVals, &*FuncArg,
                            LVMCCInfo, *ByValArg);
            ++ByValArg;
            continue;
        }
        if (IsRegLoc)
        {
            MVT RegVT = VA.getLocVT();
            size_t ArgReg = VA.getLocReg();
            const TargetRegisterClass *RC = getRegClassFor(RegVT);
            unsigned Reg = addLiveIn(DAG.getMachineFunction(), ArgReg, RC);
            SDValue ArgValue = DAG.getCopyFromReg(Chain, DL, Reg, RegVT);
            if (VA.getLocInfo() != CCValAssign::Full)
            {
                unsigned Opcode = 0;
                if (VA.getLocInfo() == CCValAssign::SExt)
                {
                    Opcode = ISD::AssertSext;
                }
                else if (VA.getLocInfo() == CCValAssign::ZExt)
                {
                    Opcode = ISD::AssertZext;
                }
                if (Opcode)
                {
                    ArgValue = DAG.getNode(Opcode, DL, RegVT, ArgValue, DAG.getValueType(ValVT));
                }
                ArgValue = DAG.getNode(ISD::TRUNCATE, DL, ValVT, ArgValue);
            }
            // Handle floating point arguments passed in integer registers. 


            InVals.push_back(ArgValue);
        }
        else
        {
            MVT LocVT = VA.getLocVT();
            assert(VA.isMemLoc());
            int FI = MFI.CreateFixedObject(ValVT.getSizeInBits() / 8, VA.getLocMemOffset(), true);
            SDValue FIN = DAG.getFrameIndex(FI, getPointerTy(DAG.getDataLayout()));
            SDValue Load = DAG.getLoad(LocVT, DL, Chain, FIN, MachinePointerInfo::getFixedStack(DAG.getMachineFunction(), FI));
            InVals.push_back(Load);
            OutChains.push_back(Load.getValue(1));
        }
    }
    for (unsigned i = 0; i < ArgLocs.size(); i++)
    {
        if (Ins[i].Flags.isSRet()) 
        {
            unsigned Reg = LVMFI->getSRetReturnReg();
            if (!Reg) {
                Reg = MF.getRegInfo().createVirtualRegister(getRegClassFor(MVT::i64));
                LVMFI->setSRetReturnReg(Reg);
            }
            SDValue Copy = DAG.getCopyToReg(DAG.getEntryNode(), DL, Reg, InVals[i]);
            Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other, Copy, Chain);
            break;
        }
    }
    if (!OutChains.empty())
    {
        OutChains.push_back(Chain);
        Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other, OutChains);
    }
    return Chain;
}

SDValue LVMTargetLowering::LowerReturn(SDValue Chain, CallingConv::ID CallConv, bool IsVarArg, \
                                       const SmallVectorImpl<ISD::OutputArg> &Outs,
                                       const SmallVectorImpl<SDValue> &OutVals,
                                       const SDLoc &DL, SelectionDAG &DAG) const 
{
    SmallVector<CCValAssign, 16> RVLocs;
    CCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), RVLocs, *DAG.getContext());
    CCInfo.AnalyzeReturn(Outs, RetCC_LVM);
    SDValue Glue;
    SmallVector<SDValue, 4> RetOps(1, Chain);
    for (unsigned i = 0, e = RVLocs.size(); i < e; ++i)
    {
        CCValAssign &VA = RVLocs[i];
        assert(VA.isRegLoc() && "Can only return in registers!");
        Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), OutVals[i], Glue);
        Glue = Chain.getValue(1);
        RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
    }
    RetOps[0] = Chain;
    if (Glue.getNode()) 
    {
        RetOps.push_back(Glue);
    }
    return DAG.getNode(LVMISD::RET_FLAG, DL, MVT::Other, RetOps);
}

void LVMTargetLowering::copyByValRegs(
    SDValue Chain, const SDLoc &DL, std::vector<SDValue> &OutChains,
    SelectionDAG &DAG, const ISD::ArgFlagsTy &Flags,
    SmallVectorImpl<SDValue> &InVals, const Argument *FuncArg, const LVMCC &CC,
    const ByValArgInfo &ByVal) const 
    {
        MachineFunction &MF = DAG.getMachineFunction();
        MachineFrameInfo &MFI = MF.getFrameInfo();
        unsigned RegAreaSize = ByVal.NumRegs * CC.regSize();
        unsigned FrameObjSize = std::max(Flags.getByValSize(), RegAreaSize);
        int FrameObjOffset;

        const ArrayRef<MCPhysReg> ByValArgRegs = CC.intArgRegs();

        if (RegAreaSize)
            FrameObjOffset = (int)CC.reservedArgArea() -
            (int)((CC.numIntArgRegs() - ByVal.FirstIdx) * CC.regSize());
        else
            FrameObjOffset = ByVal.Address;

        // Create frame object.
        EVT PtrTy = getPointerTy(DAG.getDataLayout());
        int FI = MFI.CreateFixedObject(FrameObjSize, FrameObjOffset, true);
        SDValue FIN = DAG.getFrameIndex(FI, PtrTy);
        InVals.push_back(FIN);

        if (!ByVal.NumRegs)
            return;

        // Copy arg registers.
        MVT RegTy = MVT::getIntegerVT(CC.regSize() * 8);
        const TargetRegisterClass *RC = getRegClassFor(RegTy);

        for (unsigned I = 0; I < ByVal.NumRegs; ++I) {
            unsigned ArgReg = ByValArgRegs[ByVal.FirstIdx + I];
            unsigned VReg = addLiveIn(MF, ArgReg, RC);
            unsigned Offset = I * CC.regSize();
            SDValue StorePtr = DAG.getNode(ISD::ADD, DL, PtrTy, FIN,
                                        DAG.getConstant(Offset, DL, PtrTy));
            SDValue Store = DAG.getStore(Chain, DL, DAG.getRegister(VReg, RegTy),
                                        StorePtr, MachinePointerInfo(FuncArg, Offset));
            OutChains.push_back(Store);
        }
    }

    void llvm::LVMTargetLowering::passByValArg( SDValue Chain, const SDLoc &DL, std::deque<std::pair<unsigned, SDValue>> &RegsToPass, \
        SmallVectorImpl<SDValue> &MemOpChains, SDValue StackPtr, MachineFrameInfo &MFI, SelectionDAG &DAG, SDValue Arg, const LVMCC &CC, \
        const ByValArgInfo &ByVal, const ISD::ArgFlagsTy &Flags, bool isLittle) const 
    {
        unsigned ByValSizeInBytes = Flags.getByValSize();
        unsigned OffsetInBytes = 0; // From beginning of struct
        unsigned RegSizeInBytes = CC.regSize();
        
        Align Alignment = Flags.getNonZeroByValAlign();
        EVT PtrTy = getPointerTy(DAG.getDataLayout()),
            RegTy = MVT::getIntegerVT(RegSizeInBytes * 8);

        if (ByVal.NumRegs) {
            const ArrayRef<MCPhysReg> ArgRegs = CC.intArgRegs();
            bool LeftoverBytes = (ByVal.NumRegs * RegSizeInBytes > ByValSizeInBytes);
            unsigned I = 0;

            // Copy words to registers.
            for (; I < ByVal.NumRegs - LeftoverBytes;
                ++I, OffsetInBytes += RegSizeInBytes) {
            SDValue LoadPtr = DAG.getNode(ISD::ADD, DL, PtrTy, Arg,
                                            DAG.getConstant(OffsetInBytes, DL, PtrTy));
            SDValue LoadVal = DAG.getLoad(RegTy, DL, Chain, LoadPtr,
                                            MachinePointerInfo());
            MemOpChains.push_back(LoadVal.getValue(1));
            unsigned ArgReg = ArgRegs[ByVal.FirstIdx + I];
            RegsToPass.push_back(std::make_pair(ArgReg, LoadVal));
            }

            // Return if the struct has been fully copied.
            if (ByValSizeInBytes == OffsetInBytes)
            return;

            // Copy the remainder of the byval argument with sub-word loads and shifts.
            if (LeftoverBytes) {
            assert((ByValSizeInBytes > OffsetInBytes) &&
                    (ByValSizeInBytes < OffsetInBytes + RegSizeInBytes) &&
                    "Size of the remainder should be smaller than RegSizeInBytes.");
            SDValue Val;

            for (unsigned LoadSizeInBytes = RegSizeInBytes / 2, TotalBytesLoaded = 0;
                OffsetInBytes < ByValSizeInBytes; LoadSizeInBytes /= 2) {
                unsigned RemainingSizeInBytes = ByValSizeInBytes - OffsetInBytes;

                if (RemainingSizeInBytes < LoadSizeInBytes)
                continue;

                // Load subword.
                SDValue LoadPtr = DAG.getNode(ISD::ADD, DL, PtrTy, Arg,
                                            DAG.getConstant(OffsetInBytes, DL, PtrTy));
                SDValue LoadVal = DAG.getExtLoad(ISD::ZEXTLOAD, DL, RegTy, Chain, LoadPtr,
                                                MachinePointerInfo(),
                                                MVT::getIntegerVT(LoadSizeInBytes * 8),
                                                Alignment);
                MemOpChains.push_back(LoadVal.getValue(1));

                // Shift the loaded value.
                unsigned Shamt;

                if (isLittle)
                Shamt = TotalBytesLoaded * 8;
                else
                Shamt = (RegSizeInBytes - (TotalBytesLoaded + LoadSizeInBytes)) * 8;

                SDValue Shift = DAG.getNode(ISD::SHL, DL, RegTy, LoadVal,
                                            DAG.getConstant(Shamt, DL, MVT::i32));

                if (Val.getNode())
                Val = DAG.getNode(ISD::OR, DL, RegTy, Val, Shift);
                else
                Val = Shift;

                OffsetInBytes += LoadSizeInBytes;
                TotalBytesLoaded += LoadSizeInBytes;
            }

            unsigned ArgReg = ArgRegs[ByVal.FirstIdx + I];
            RegsToPass.push_back(std::make_pair(ArgReg, Val));
            return;
            } // end of if (LeftoverBytes)
        }

        // Copy remainder of byval arg to it with memcpy.
        unsigned MemCpySize = ByValSizeInBytes - OffsetInBytes;
        SDValue Src = DAG.getNode(ISD::ADD, DL, PtrTy, Arg,
                                    DAG.getConstant(OffsetInBytes, DL, PtrTy));
        SDValue Dst = DAG.getNode(ISD::ADD, DL, PtrTy, StackPtr,
                                    DAG.getIntPtrConstant(ByVal.Address, DL));
        Chain = DAG.getMemcpy(Chain, DL, Dst, Src,
                                DAG.getConstant(MemCpySize, DL, PtrTy), Alignment,
                                /*isVolatile*/false, /*AlwaysInline*/false,
                                /*isTailCall*/false,
                                MachinePointerInfo(), MachinePointerInfo());
        MemOpChains.push_back(Chain);
    }

    SDValue llvm::LVMTargetLowering::passArgOnStack(SDValue StackPtr, unsigned Offset, SDValue Chain, SDValue Arg, \
        const SDLoc &DL, bool IsTailCall, SelectionDAG &DAG) const
    {
        if (!IsTailCall) {
            SDValue PtrOff = DAG.getNode(ISD::ADD, DL, getPointerTy(DAG.getDataLayout()),
                                        StackPtr, DAG.getIntPtrConstant(Offset, DL));
            return DAG.getStore(Chain, DL, Arg, PtrOff, MachinePointerInfo());
        }

        MachineFrameInfo &MFI = DAG.getMachineFunction().getFrameInfo();
        int FI = MFI.CreateFixedObject(Arg.getValueSizeInBits() / 8, Offset, false);
        SDValue FIN = DAG.getFrameIndex(FI, getPointerTy(DAG.getDataLayout()));
        return DAG.getStore(Chain, DL, Arg, FIN, MachinePointerInfo(),
                            /* Alignment = */ 0, MachineMemOperand::MOVolatile);
    }

    SDValue llvm::LVMTargetLowering::getGlobalReg(SelectionDAG &DAG, EVT Ty) const
    {
          LVMMachineFunctionInfo *FI = DAG.getMachineFunction().getInfo<LVMMachineFunctionInfo>();
            return DAG.getRegister(FI->getGlobalBaseReg(), Ty);
    }

    void llvm::LVMTargetLowering::getOpndList( SmallVectorImpl<SDValue> &Ops, std::deque<std::pair<unsigned, SDValue>> &RegsToPass, bool IsPICCall,
        bool GlobalOrExternal, bool InternalLinkage, CallLoweringInfo &CLI, SDValue Callee, SDValue Chain) const
    {
        // T9 should contain the address of the callee function if
        // -relocation-model=pic or it is an indirect call.
        Ops.push_back(Callee);

        // // Insert node "GP copy globalreg" before call to function.
        // //
        // // R_CPU0_CALL* operators (emitted when non-internal functions are called
        // // in PIC mode) allow symbols to be resolved via lazy binding.
        // // The lazy binding stub requires GP to point to the GOT.
        // if (IsPICCall && !InternalLinkage) {
        //     unsigned GPReg = LVM::GP;
        //     EVT Ty = MVT::i32;
        //     RegsToPass.push_back(std::make_pair(GPReg, getGlobalReg(CLI.DAG, Ty)));
        // }

        // Build a sequence of copy-to-reg nodes chained together with token
        // chain and flag operands which copy the outgoing args into registers.
        // The InFlag in necessary since all emitted instructions must be stuck
        // together.
        SDValue InFlag;

        for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i) {
            Chain = CLI.DAG.getCopyToReg(Chain, CLI.DL, RegsToPass[i].first,
                                        RegsToPass[i].second, InFlag);
            InFlag = Chain.getValue(1);
        }

        // Add argument registers to the end of the list so that they are known
        // live into the call.
        for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i)
            Ops.push_back(CLI.DAG.getRegister(RegsToPass[i].first,
                                            RegsToPass[i].second.getValueType()));

        // Add a register mask operand representing the call-preserved registers.
        const TargetRegisterInfo *TRI = Subtarget.getRegisterInfo();
        const uint32_t *Mask = TRI->getCallPreservedMask(CLI.DAG.getMachineFunction(),
                                                        CLI.CallConv);
        assert(Mask && "Missing call preserved mask for calling convention");
        Ops.push_back(CLI.DAG.getRegisterMask(Mask));

        if (InFlag.getNode())
            Ops.push_back(InFlag);
    }

    LVMTargetLowering::LVMCC::LVMCC(CallingConv::ID CallConv, CCState &Info, SpecialCallingConvType SpecialCallingConv) : CCInfo(Info), CallConv(CallConv) 
    {
        CCInfo.AllocateStack(reservedArgArea(), Align(1));
    }

void llvm::LVMTargetLowering::LVMCC::analyzeFormalArguments(const SmallVectorImpl<ISD::InputArg> &Ins, bool IsSoftFloat, Function::const_arg_iterator FuncArg) 
{
    unsigned NumArgs = Ins.size();
    llvm::CCAssignFn *FixedFn = fixedArgFn();
    unsigned CurArgIdx = 0;
    for (unsigned I = 0; I != NumArgs; I++)
    {
        MVT ArgVT = Ins[I].VT;
        ISD::ArgFlagsTy ArgFlags = Ins[I].Flags;
        if (Ins[I].isOrigArg()) {
            std::advance(FuncArg, Ins[I].getOrigArgIndex() - CurArgIdx);
            CurArgIdx = Ins[I].getOrigArgIndex();
        }
        CurArgIdx = Ins[I].getOrigArgIndex();
        if (ArgFlags.isByVal()) {
        handleByValArg(I, ArgVT, ArgVT, CCValAssign::Full, ArgFlags);
        continue;
        }
        MVT RegVT = getRegVT(ArgVT, FuncArg->getType(), nullptr, IsSoftFloat);
        if (!FixedFn(I, ArgVT, RegVT, CCValAssign::Full, ArgFlags, CCInfo)) continue;
        llvm_unreachable(nullptr);
    }
}

void llvm::LVMTargetLowering::LVMCC::analyzeCallOperands(const SmallVectorImpl<ISD::OutputArg> &Args, bool IsValArg,
    bool IsSoftFloat, const SDNode *CallNode, std::vector<ArgListEntry> &FuncArgs)
{
    assert((CallConv != CallingConv::Fast || !IsValArg) && "CallingConv::Fast shouldn't be used for vararg functions.");

    unsigned NumOpnds = Args.size();
    llvm::CCAssignFn *FixedFn = fixedArgFn();

    for (unsigned I = 0; I != NumOpnds; ++I) {
        MVT ArgVT = Args[I].VT;
        ISD::ArgFlagsTy ArgFlags = Args[I].Flags;
        bool R;

        if (ArgFlags.isByVal()) {
        handleByValArg(I, ArgVT, ArgVT, CCValAssign::Full, ArgFlags);
        continue;
        }

        {
        MVT RegVT = getRegVT(ArgVT, FuncArgs[Args[I].OrigArgIndex].Ty, CallNode,
                            IsSoftFloat);
        R = FixedFn(I, ArgVT, RegVT, CCValAssign::Full, ArgFlags, CCInfo);
        }

        if (R) {
#ifndef NDEBUG
        dbgs() << "Call operand #" << I << " has unhandled type "
                << EVT(ArgVT).getEVTString();
#endif
        llvm_unreachable("unknow how to process");
        }

    }
}

void llvm::LVMTargetLowering::LVMCC::analyzeCallResult(const SmallVectorImpl<ISD::InputArg> &Ins, bool IsSoftFloat,
    const SDNode *CallNode, const Type *RetTy) const
{
    analyzeReturn(Ins, IsSoftFloat, CallNode, RetTy);
}

void llvm::LVMTargetLowering::LVMCC::analyzeReturn(const SmallVectorImpl<ISD::OutputArg> &Outs, bool IsSoftFloat,
    const Type *RetTy) const
{
    analyzeReturn(Outs, IsSoftFloat, nullptr, RetTy);
}
template<typename Ty>
void llvm::LVMTargetLowering::LVMCC::analyzeReturn(
    const SmallVectorImpl<Ty> &RetVals, bool IsSoftFloat,
    const SDNode *CallNode, const Type *RetTy) const
{
    CCAssignFn *Fn;

    Fn = RetCC_LVM;

    for (unsigned I = 0, E = RetVals.size(); I < E; ++I) {
        MVT VT = RetVals[I].VT;
        ISD::ArgFlagsTy Flags = RetVals[I].Flags;
        MVT RegVT = this->getRegVT(VT, RetTy, CallNode, IsSoftFloat);

        if (Fn(I, VT, RegVT, CCValAssign::Full, Flags, this->CCInfo)) {
#ifndef NDEBUG
        dbgs() << "Call result #" << I << " has unhandled type "
                << EVT(VT).getEVTString() << '\n';
#endif
        llvm_unreachable("unknow how to process");
        }
    }
}

unsigned llvm::LVMTargetLowering::LVMCC::numIntArgRegs() const
{
    return 2;
}

unsigned LVMTargetLowering::LVMCC::reservedArgArea() const { return 8; }

const ArrayRef<MCPhysReg> LVMTargetLowering::LVMCC::intArgRegs() const 
{
    return Regs64;
}

void llvm::LVMTargetLowering::LVMCC::handleByValArg(unsigned ValNo, MVT ValVT, MVT LocVT,
 CCValAssign::LocInfo LocInfo, ISD::ArgFlagsTy ArgFlags) 
    {
        assert(ArgFlags.getByValSize() && "Byval argument's size shouldn't be 0.");
        ByValArgInfo ByVal;
        unsigned RegSize = regSize();
        unsigned ByValSize =alignTo(ArgFlags.getByValSize(), RegSize);
        unsigned align = std::min(std::max(ArgFlags.getByValSize(), RegSize), RegSize * 2);
        if (useRegsForByval())
        {
            allocateRegs(ByVal, ByValSize, align);
        }
        ByVal.Address = CCInfo.AllocateStack(ByValSize - RegSize * ByVal.NumRegs, (Align)align);
        CCInfo.addLoc(CCValAssign::getMem(ValNo, ValVT, ByVal.Address, LocVT, LocInfo));
        ByValArgs.push_back(ByVal);
    }

CCAssignFn *LVMTargetLowering::LVMCC::fixedArgFn() const { return CC_LVM; }

void llvm::LVMTargetLowering::LVMCC::allocateRegs(ByValArgInfo &ByVal, unsigned ByValSize, unsigned Align) 
{
    unsigned RegSize = regSize(), NumIntArgRegs = numIntArgRegs();
    const ArrayRef<MCPhysReg> IntArgRegs = intArgRegs();
    assert(!(ByValSize % RegSize) && !(Align % RegSize) &&
         "Byval argument's size and alignment should be a multiple of"
         "RegSize.");
    ByVal.FirstIdx = CCInfo.getFirstUnallocated(IntArgRegs); 
    if ((Align > RegSize) && (ByVal.FirstIdx % 2))
    {
        CCInfo.AllocateReg(IntArgRegs[ByVal.FirstIdx]);
        ByVal.FirstIdx++;
    }

    for (unsigned I = ByVal.FirstIdx; ByValSize && (I << NumIntArgRegs); ByValSize -= RegSize, I++, ByVal.NumRegs++)
    {
        CCInfo.AllocateReg(IntArgRegs[I]);
    }
}

MVT llvm::LVMTargetLowering::LVMCC::getRegVT(MVT VT, const Type *OrigTy,
                                             const SDNode *CallNode,
                                             bool IsSoftFloat) const {
    return VT; 
}

SDValue LVMTargetLowering::getTargetNode(GlobalAddressSDNode *N, EVT Ty,
                                          SelectionDAG &DAG,
                                          unsigned Flag) const {
  return DAG.getTargetGlobalAddress(N->getGlobal(), SDLoc(N), Ty, 0, Flag);
}

SDValue LVMTargetLowering::getTargetNode(ExternalSymbolSDNode *N, EVT Ty,
                                          SelectionDAG &DAG,
                                          unsigned Flag) const {
  return DAG.getTargetExternalSymbol(N->getSymbol(), Ty, Flag);
}

SDValue LVMTargetLowering::getTargetNode(BlockAddressSDNode *N, EVT Ty,
                                          SelectionDAG &DAG,
                                          unsigned Flag) const {
  return DAG.getTargetBlockAddress(N->getBlockAddress(), Ty, 0, Flag);
}

SDValue LVMTargetLowering::getTargetNode(JumpTableSDNode *N, EVT Ty,
                                          SelectionDAG &DAG,
                                          unsigned Flag) const {
  return DAG.getTargetJumpTable(N->getIndex(), Ty, Flag);
}