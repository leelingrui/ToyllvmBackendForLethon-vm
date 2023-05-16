#ifndef LLVM_LIB_TARGET_LVM_LVMISELLOWERING_H
#define LLVM_LIB_TARGET_LVM_LVMISELLOWERING_H
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "LVM.h"
#include "LVMMachineFunctionInfo.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/TargetLowering.h"
#include "MCTargetDesc/LVMBaseInfo.h"
#include <deque>
namespace llvm 
{
    class LVMSubtarget;
    class LVMTargetMachine;
    namespace LVMISD {
        enum NodeType {
            FIRST_NUMBER = ISD::BUILTIN_OP_END,
            RET_FLAG,
            JMPLINK,
            CALL,
            TAILCALL,
            Wrapper,
            Lo,
            Hi
        };
    }

    class LVMTargetLowering : public TargetLowering
    {
    private:
        const LVMSubtarget &Subtarget;
        // Create a TargetGlobalAddress node.
        SDValue getTargetNode(GlobalAddressSDNode *N, EVT Ty, SelectionDAG &DAG, unsigned Flag) const;

        // Create a TargetExternalSymbol node.
        SDValue getTargetNode(ExternalSymbolSDNode *N, EVT Ty, SelectionDAG &DAG, unsigned Flag) const;

        // Create a TargetBlockAddress node.
        SDValue getTargetNode(BlockAddressSDNode *N, EVT Ty, SelectionDAG &DAG, unsigned Flag) const;

        // Create a TargetJumpTable node.
        SDValue getTargetNode(JumpTableSDNode *N, EVT Ty, SelectionDAG &DAG, unsigned Flag) const;
    protected:
        struct ByValArgInfo {
            unsigned FirstIdx;  // Index of the first register used.
            unsigned NumRegs;   // Number of registers used for this argument.
            unsigned Address;   // Offset of the stack area used to pass this argument.

            ByValArgInfo() : FirstIdx(0), NumRegs(0), Address(0) { }
        };
        class LVMCC
        {
        public:
            enum SpecialCallingConvType {
                NoSpecialCallingConv
            };

            LVMCC(CallingConv::ID CallConv, CCState &Info,
                SpecialCallingConvType SpecialCallingConv = NoSpecialCallingConv);

            void analyzeFormalArguments(const SmallVectorImpl<ISD::InputArg> &Ins,
                                        bool IsSoftFloat,
                                        Function::const_arg_iterator FuncArg);
            void analyzeCallOperands(const SmallVectorImpl<ISD::OutputArg> &Outs,
                                    bool IsVarArg, bool IsSoftFloat,
                                    const SDNode *CallNode,
                                    std::vector<ArgListEntry> &FuncArgs);
            void analyzeCallResult(const SmallVectorImpl<ISD::InputArg> &Ins,
                                bool IsSoftFloat, const SDNode *CallNode,
                                const Type *RetTy) const;

            void analyzeReturn(const SmallVectorImpl<ISD::OutputArg> &Outs,
                            bool IsSoftFloat, const Type *RetTy) const;
            template<typename Ty>
            void analyzeReturn(const SmallVectorImpl<Ty> &RetVals, bool IsSoftFloat,
                      const SDNode *CallNode, const Type *RetTy) const;
            const CCState &getCCInfo() const { return CCInfo; }

            // Returns true if function has byval arguments.
            bool hasByValArg() const { return !ByValArgs.empty(); }

            // regSize - Size (in number of bits) of integer registers.
            unsigned regSize() const { return 8; }
            // numIntArgRegs - Number of integer registers available for calls.
            unsigned numIntArgRegs() const;

            // The size of the area the caller reserves for register arguments.
            // This is 16-byte if ABI is O32.
            unsigned reservedArgArea() const;

            // Return pointer to array of integer argument registers.
            const ArrayRef<MCPhysReg> intArgRegs() const;

            typedef SmallVectorImpl<ByValArgInfo>::const_iterator byval_iterator;
            byval_iterator byval_begin() const { return ByValArgs.begin(); }
            byval_iterator byval_end() const { return ByValArgs.end(); }

        private:
            void handleByValArg(unsigned ValNo, MVT ValVT, MVT LocVT,
                                CCValAssign::LocInfo LocInfo,
                                ISD::ArgFlagsTy ArgFlags);

            // useRegsForByval - Returns true if the calling convention allows the
            // use of registers to pass byval arguments.
            bool useRegsForByval() const { return CallConv != CallingConv::Fast; }

            // Return the function that analyzes fixed argument list functions.
            llvm::CCAssignFn *fixedArgFn() const;

            void allocateRegs(ByValArgInfo &ByVal, unsigned ByValSize, unsigned Align);

            // Return the type of the register which is used to pass an argument or
            // return a value. This function returns f64 if the argument is an i64
            // value whihc has been generated as a result of softening an f128 value.
            // otherwise, it just returns VT.
            MVT getRegVT(MVT VT, const Type *OrigTy, const SDNode *CallNode,
                        bool IsSoftFloat) const;
            CCState &CCInfo;
            CallingConv::ID CallConv;
            SmallVector<ByValArgInfo, 2> ByValArgs;
        };
    public:
        LVMCC::SpecialCallingConvType getSpecialCallingConv(SDValue Callee) const;
        virtual bool isEligibleForTailCallOptimization(const LVMCC &LVMCCInfo,
                                    unsigned NextStackOffset, const LVMMachineFunctionInfo &FI) const;
        SDValue LowerCall(TargetLowering::CallLoweringInfo &CLI, SmallVectorImpl<SDValue> &InVals) const override;
        SDValue LowerCallResult(SDValue Chain, SDValue InFlag, CallingConv::ID CallConv, bool isVarArg,
                        const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &dl, SelectionDAG &DAG,
                        SmallVectorImpl<SDValue> &InVals, const SDNode *CallNode, const Type *RetTy) const;
        
        explicit LVMTargetLowering(LVMTargetMachine &TM);
        const char *getTargetNodeName(unsigned Opcode) const override;
        SDValue LowerFormalArguments(SDValue Chain, CallingConv::ID CallConv, bool IsVarArg, const SmallVectorImpl<ISD::InputArg> &Ins, \
        const SDLoc &DL, SelectionDAG &DAG,
        SmallVectorImpl<SDValue> &InVals) const override;
        SDValue LowerReturn(SDValue Chain, CallingConv::ID CallConv, bool IsVarArg, const SmallVectorImpl<ISD::OutputArg> &Outs,\
            const SmallVectorImpl<SDValue> &OutVals, const SDLoc &DL, SelectionDAG &DAG) const override;
        void copyByValRegs(SDValue Chain, const SDLoc &DL, std::vector<SDValue> &OutChains,
              SelectionDAG &DAG, const ISD::ArgFlagsTy &Flags,
              SmallVectorImpl<SDValue> &InVals, const Argument *FuncArg,
              const LVMCC &CC, const ByValArgInfo &ByVal) const;
        void passByValArg(SDValue Chain, const SDLoc &DL,
                    std::deque<std::pair<unsigned, SDValue>> &RegsToPass,
                    SmallVectorImpl<SDValue> &MemOpChains, SDValue StackPtr,
                    MachineFrameInfo &MFI, SelectionDAG &DAG, SDValue Arg,
                    const LVMCC &CC, const ByValArgInfo &ByVal,
                    const ISD::ArgFlagsTy &Flags, bool isLittle) const;
        SDValue passArgOnStack(SDValue StackPtr, unsigned Offset, SDValue Chain,
                         SDValue Arg, const SDLoc &DL, bool IsTailCall,
                         SelectionDAG &DAG) const;
    protected:
        SDValue getGlobalReg(SelectionDAG &DAG, EVT Ty) const;
        template<class NodeTy>
        SDValue getAddrLocal(NodeTy *N, EVT Ty, SelectionDAG &DAG) const
        {
            SDLoc DL(N);
            unsigned GOTFlag = LVMII::MO_GOT;
            SDValue GOT = DAG.getNode(LVMISD::Wrapper, DL, Ty, getGlobalReg(DAG, Ty),
                                    getTargetNode(N, Ty, DAG, GOTFlag));
            SDValue Load = DAG.getLoad(Ty, DL, DAG.getEntryNode(), GOT,
                            MachinePointerInfo::getGOT(DAG.getMachineFunction()));
            unsigned LoFlag = LVMII::MO_ABS_LO;
            SDValue Lo = DAG.getNode(LVMISD::Lo, DL, Ty,
                                    getTargetNode(N, Ty, DAG, LoFlag));
            return DAG.getNode(ISD::ADD, DL, Ty, Load, Lo);
        }
        template<class NodeTy>
        SDValue getAddrGlobal(NodeTy *N, EVT Ty, SelectionDAG &DAG, unsigned Flag, SDValue Chain,
                                const MachinePointerInfo &PtrInfo) const {
            SDLoc DL(N);
            SDValue Tgt = DAG.getNode(LVMISD::Wrapper, DL, Ty, getGlobalReg(DAG, Ty),
                                    getTargetNode(N, Ty, DAG, Flag));
            return DAG.getLoad(Ty, DL, Chain, Tgt, PtrInfo);
        }
        virtual void getOpndList(SmallVectorImpl<SDValue> &Ops, std::deque<std::pair<unsigned, SDValue>> &RegsToPass,
              bool IsPICCall, bool GlobalOrExternal, bool InternalLinkage, CallLoweringInfo &CLI, SDValue Callee, SDValue Chain) const;
    };
} // end namespace llvm
#endif