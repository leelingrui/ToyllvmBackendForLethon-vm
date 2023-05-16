#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetMachine.h"
#include "LVMISelDAGToDAG.h"

#define DEBUG_TYPE "LVM-ISEL"
namespace llvm
{
    bool LVMDAGToDAGISel::SelectAddr(SDNode *Parent, SDValue Addr, SDValue &Base, SDValue &Offset) {
        EVT ValTy = Addr.getValueType();
        SDLoc DL(Addr);

        // If Parent is an unaligned f32 load or store, select a (base + index)
        // float point load/store instruction (luxcl or suxcl)
        const LSBaseSDNode *LS = 0;

        if (Parent && (LS = dyn_cast<LSBaseSDNode>(Parent)))
        {
            EVT VT = LS->getMemoryVT();

            if (VT.getSizeInBits() / 8 > LS->getAlignment())
            {
                assert(false && "Unaligned loads/stores not supported for this type.");
                if (VT == MVT::f32)
                    return false;
            }
        }

        // If address is FI, get the TargetFrameIndex.
        if (FrameIndexSDNode *FIN = dyn_cast<FrameIndexSDNode>(Addr)) {
            Base = CurDAG->getTargetFrameIndex(FIN->getIndex(), ValTy);
            Offset = CurDAG->getTargetConstant(0, DL, ValTy);
            return true;
        }

        // on PIC code Load GA
        // if (Addr.getOpcode() == LVMISD::Wrapper) {
        //     Base = Addr.getOperand(0);
        //     Offset = Addr.getOperand(1);
        //     return true;
        // }

        // static
        if (TM.getRelocationModel() != Reloc::PIC_) {
            if ((Addr.getOpcode() == ISD::TargetExternalSymbol ||
                Addr.getOpcode() == ISD::TargetGlobalAddress))
            return false;
        }

        // adderss of base + const or fi + const
        if (CurDAG->isBaseWithConstantOffset(Addr)) {
            ConstantSDNode *CN = dyn_cast<ConstantSDNode>(Addr.getOperand(1));
            if (isInt<16>(CN->getSExtValue())) {
            // If the first operand is a FI, get the TargetFI Node
            if (FrameIndexSDNode *FIN = dyn_cast<FrameIndexSDNode>(Addr.getOperand(0)))
                Base = CurDAG->getTargetFrameIndex(FIN->getIndex(), ValTy);
            else
                Base = Addr.getOperand(0);

            Offset = CurDAG->getTargetConstant(CN->getZExtValue(), DL, ValTy);
            return true;
            }
        }

        Base = Addr;
        Offset = CurDAG->getTargetConstant(0, DL, ValTy);
        return true;
    }

    bool LVMDAGToDAGISel::SelectAddr_I(SDNode *Parent, SDValue Addr, SDValue &Base, SDValue &Offset) {
        EVT ValTy = Addr.getValueType();
        SDLoc DL(Addr);

        // If Parent is an unaligned f32 load or store, select a (base + index)
        // float point load/store instruction (luxcl or suxcl)
        const LSBaseSDNode *LS = 0;

        if (Parent && (LS = dyn_cast<LSBaseSDNode>(Parent)))
        {
            EVT VT = LS->getMemoryVT();

            if (VT.getSizeInBits() / 8 > LS->getAlignment())
            {
                assert(false && "Unaligned loads/stores not supported for this type.");
                if (VT == MVT::f32)
                    return false;
            }
        }

        // If address is FI, get the TargetFrameIndex.
        if (FrameIndexSDNode *FIN = dyn_cast<FrameIndexSDNode>(Addr)) {
            Base = CurDAG->getTargetFrameIndex(FIN->getIndex(), ValTy);
            Offset = CurDAG->getTargetConstant(0, DL, ValTy);
            return true;
        }

        // on PIC code Load GA
        // if (Addr.getOpcode() == LVMISD::Wrapper) {
        //     Base = Addr.getOperand(0);
        //     Offset = Addr.getOperand(1);
        //     return true;
        // }

        // static
        if (TM.getRelocationModel() != Reloc::PIC_) {
            if ((Addr.getOpcode() == ISD::TargetExternalSymbol ||
                Addr.getOpcode() == ISD::TargetGlobalAddress))
            return false;
        }

        // adderss of base + const or fi + const
        if (CurDAG->isBaseWithConstantOffset(Addr)) {
            ConstantSDNode *CN = dyn_cast<ConstantSDNode>(Addr.getOperand(1));
            if (isInt<16>(CN->getSExtValue())) {
            // If the first operand is a FI, get the TargetFI Node
            if (FrameIndexSDNode *FIN = dyn_cast<FrameIndexSDNode>(Addr.getOperand(0)))
                Base = CurDAG->getTargetFrameIndex(FIN->getIndex(), ValTy);
            else
                Base = Addr.getOperand(0);

            Offset = CurDAG->getTargetConstant(CN->getZExtValue(), DL, ValTy);
            return true;
            }
        }

        Base = Addr;
        Offset = CurDAG->getTargetConstant(0, DL, ValTy);
        return true;
    }

    void LVMDAGToDAGISel::Select(SDNode *N) 
    {
        SDLoc DL(N);
        unsigned Opcode = N->getOpcode();
        if (N->isMachineOpcode()) 
        {
            LLVM_DEBUG(errs() << "== "; N->dump(CurDAG); errs() << "\n");
            N->setNodeId(-1);
            return;
        }
        switch (Opcode) {
        // case ISD::Constant: 
        // {
            
        //     // LVMAnalyzeImmediate analyzer;
        //     // analyzer.Analyze(Imm);
        //     const ConstantSDNode *CN = dyn_cast<ConstantSDNode>(N);
        //     int64_t Imm = CN->getSExtValue();
        //     // ReplaceNode(N, CurDAG->getTargetConstant(Imm, SDLoc(N), N->getValueType(0)));
        //     // return;
        //     if (-128 <= Imm && Imm <= 127) 
        //     {                
        //         return; 
        //     }
        // }
        // case ISD::STORE:
        // {
        //     LLVM_DEBUG(dbgs() << "ISEL: Starting selection on root node: "; N->dump(); dbgs() << "\n");
        //     SDValue SDDst = N->getOperand(2);
        //     SDValue SDVal = N->getOperand(1);
        //     LLVM_DEBUG(dbgs() << "ISEL: Dstnode: "; SDDst->dump(); dbgs() << "\n");
        //     LLVM_DEBUG(dbgs() << "ISEL: Valnode: "; SDVal->dump(); dbgs() << "\n");
        //     SDNode *Result = CurDAG->getMachineNode(LVM::STR_64I, DL, MVT::i8, SDDst, SDVal);
        //     return;
        // }
        // case ISD::LOAD:
        // {
        //     LLVM_DEBUG(dbgs() << "ISEL: Starting selection on root node: "; N->dump(); dbgs() << "\n");
        //     SDValue SDDst = N->getOperand(2);
        //     SDValue SDVal = N->getOperand(1);
        //     LLVM_DEBUG(dbgs() << "ISEL: Dstnode: "; SDDst->dump(); dbgs() << "\n");
        //     LLVM_DEBUG(dbgs() << "ISEL: Valnode: "; SDVal->dump(); dbgs() << "\n");
        //     SDNode *Result = CurDAG->getMachineNode(LVM::LDR_64I, DL, MVT::i8, SDDst, SDVal);
        //     return;
        // }
        // case ISD::FrameIndex:
        // {
            
        // }
        default:
            break;
        }
        SelectCode(N);
    }

    SDNode *LVMDAGToDAGISel::getGlobalBaseReg() 
    { 
        unsigned GlobalBaseReg = MF->getInfo<LVMMachineFunctionInfo>()->getGlobalBaseReg();
        return CurDAG->getRegister(GlobalBaseReg, getTargetLowering()->getPointerTy(
                                                CurDAG->getDataLayout())).getNode();
    }

    FunctionPass *createLVMISelDag(LVMTargetMachine &TM)
    {
        return new LVMDAGToDAGISel(TM);
    }
}