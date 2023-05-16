#include "LVMTargetMachine.h"
#include "LVM.h"
#include "TargetInfo/LVMTargetInfo.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/MC/TargetRegistry.h"


namespace llvm 
{
    class LVMPassConfig : public TargetPassConfig 
    {
    public:
        LVMPassConfig(LVMTargetMachine &TM, PassManagerBase &PM) : TargetPassConfig(TM, PM) {}
        bool addInstSelector() override {
            addPass(createLVMISelDag(getLVMTargetMachine()));
            return false;
        }
    private:
        LVMTargetMachine &getLVMTargetMachine() const {
            return getTM<LVMTargetMachine>();
        }
    };

    StringRef computeDataLayout(const Triple &TT) {
        assert(TT.isArch64Bit() && "Only LVM64 is currently supported!");
        return "e-m:e-p:64:64-i1:8-i8:8-i16:16-i32:32-i64:64-n8:16:32:64-S64";
    }

    Reloc::Model getEffectiveRelocModel(Optional<Reloc::Model> RM) {
        return RM.hasValue() ? *RM : Reloc::Static;
    }

    LVMTargetMachine::LVMTargetMachine(const Target &T, const Triple &TT, StringRef CPU, StringRef FS, const TargetOptions &Options, Optional<Reloc::Model> RM, Optional<CodeModel::Model> CM, CodeGenOpt::Level OL, bool JIT) :\
     LLVMTargetMachine(T, computeDataLayout(TT), TT, CPU, FS, Options, getEffectiveRelocModel(RM), getEffectiveCodeModel(CM, CodeModel::Small), OL), Subtarget(TT, CPU, CPU, FS, *this), TLOF(std::make_unique<TargetLoweringObjectFileELF>()) 
    { 
        initAsmInfo();
    }

    TargetPassConfig *LVMTargetMachine::createPassConfig(PassManagerBase &PM) {
        return new LVMPassConfig(*this, PM);
    }

    extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeLVMTarget()
    {
        RegisterTargetMachine<LVMTargetMachine> X(getLVMTarget());
    }


}