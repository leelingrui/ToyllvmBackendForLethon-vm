#ifndef LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMMCASMINFO_H
#define LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMMCASMINFO_H

#include "llvm/MC/MCAsmInfoELF.h"

namespace llvm {

class Triple;

    class LVMMCAsmInfo : public MCAsmInfoELF {
    public:
        explicit LVMMCAsmInfo(const Triple &TT);
    };

} // end namespace llvm

#endif // LLVM_LIB_TARGET_LVM_MCTARGETDESC_LVMMCASMINFO_H