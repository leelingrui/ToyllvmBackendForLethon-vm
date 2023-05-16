#ifndef LLVM_LIB_TARGET_LVM_LVM_H
#define LLVM_LIB_TARGET_LVM_LVM_H

namespace llvm 
{

    class LVMTargetMachine;
    class FunctionPass;

    FunctionPass *createLVMISelDag(LVMTargetMachine &TM);
}
 #endif