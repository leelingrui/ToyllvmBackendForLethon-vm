#ifndef LLVM_LIB_TARGET_LVM_LVMISELDAGTODAG_H
#define LLVM_LIB_TARGET_LVM_LVMISELDAGTODAG_H
#include "MCTargetDesc/LVMMCTargetDesc.h"
#include "LVM.h"
#include "LVMTargetMachine.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "LVMMachineFunctionInfo.h"
#include "llvm/Support/Debug.h"
#include "LVMAnalyzeImmediate.h"

namespace llvm 
{
    class LVMDAGToDAGISel : public SelectionDAGISel {
    public:
        explicit LVMDAGToDAGISel(LVMTargetMachine &TM) : SelectionDAGISel(TM) {}
        StringRef getPassName() const override {
            return "LVM DAG->DAG Pattern Instruction Selection";
        }
        bool SelectAddr(SDNode *Parent, SDValue Addr, SDValue &Base, SDValue &Offset);
        bool SelectAddr_I(SDNode *Parent, SDValue Addr, SDValue &Base, SDValue &Offset);

        void Select(SDNode *N) override;
        SDNode* getGlobalBaseReg();
    private:
    #include "LVMGenDAGISel.inc"
    };

}

#endif