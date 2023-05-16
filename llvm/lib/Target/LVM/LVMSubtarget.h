#ifndef LLVM_LIB_TARGET_LVM_LVMSUBTARGET_H
#define LLVM_LIB_TARGET_LVM_LVMSUBTARGET_H
#include "LVMFrameLowering.h"
#include "LVMISelLowering.h"
#include "LVMInstrInfo.h"
#include "LVMRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "MCTargetDesc/LVMABIInfo.h"
#define GET_SUBTARGETINFO_HEADER
#include "LVMGenSubtargetInfo.inc"

namespace llvm
{
    class LVMTargetMachine;

    class LVMSubtarget : public LVMGenSubtargetInfo {
        LVMTargetLowering TLInfo;
        LVMFrameLowering FrameLowering;
        LVMInstrInfo InstrInfo;
        LVMRegisterInfo RegInfo;
        LVMABIInfo ABI;
        bool IsLittle;
    public:
        bool isLittle() const { return IsLittle; }
        bool abiUsesSoftFloat() const;
        LVMSubtarget(const Triple &TT, StringRef CPU, StringRef TuneCPU, StringRef FS, LVMTargetMachine &_TM);
        const LVMTargetLowering *getTargetLowering() const override {
            return &TLInfo;
        }
        const LVMABIInfo &getABI() const;
        const LVMFrameLowering *getFrameLowering() const override {
            return &FrameLowering;
        }
        const LVMInstrInfo *getInstrInfo() const override { return &InstrInfo; }
        const LVMRegisterInfo *getRegisterInfo() const override {
            return &RegInfo;
        }

    private:
        void ParseSubtargetFeatures(StringRef CPU, StringRef TuneCPU, StringRef FS);
        LVMSubtarget &initializeSubtargetDependencies(const Triple &TT, StringRef CPU, StringRef FS);
    };
} // namespace llvm




#endif