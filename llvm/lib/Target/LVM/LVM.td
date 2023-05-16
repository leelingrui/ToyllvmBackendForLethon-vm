include "llvm/Target/Target.td"
include "LVMRegisterInfo.td"
include "LVMInstructionInfo.td"
include "LVMCallingConv.td"

def : ProcessorModel<"lethon-64", NoSchedModel, []>;

def LVMInstrInfo : InstrInfo;
def LVM : Target {
    let InstructionSet = LVMInstrInfo;
}