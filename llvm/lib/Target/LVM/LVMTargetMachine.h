#ifndef LLVM_LIB_TARGET_LVM_LVMTARGETMACHINE_H
#define LLVM_LIB_TARGET_LVM_LVMTARGETMACHINE_H

#include "LVMSubtarget.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm 
{
    class LVMTargetMachine : public LLVMTargetMachine {
        LVMSubtarget Subtarget;
        std::unique_ptr<TargetLoweringObjectFile> TLOF;

    public:
        LVMTargetMachine(const Target &T, const Triple &TT, StringRef CPU, StringRef FS, const TargetOptions &Options, Optional<Reloc::Model> RM, Optional<CodeModel::Model> CM, CodeGenOpt::Level OL, bool JIT);
        const LVMSubtarget *getSubtargetImpl(const Function &F) const override {
            return &Subtarget;
        }
        const LVMSubtarget *getSubtargetImpl() const { return &Subtarget; }
        TargetPassConfig *createPassConfig(PassManagerBase &PM) override;
        TargetLoweringObjectFile *getObjFileLowering() const override 
        {
            return TLOF.get();
        }
    };

}

#endif