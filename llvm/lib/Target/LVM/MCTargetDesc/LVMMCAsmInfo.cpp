#include "LVMMCAsmInfo.h"
#include "llvm/ADT/Triple.h"

using namespace llvm;

LVMMCAsmInfo::LVMMCAsmInfo(const Triple &TT) {
    assert(TT.isArch64Bit() && "Only LVM64 is currently supported!");
    CodePointerSize = CalleeSaveStackSlotSize = TT.isArch64Bit() ? 8 : 8;
    CommentString = "#";
    Data16bitsDirective = "\t.half\t";
    Data32bitsDirective = "\t.word\t";
}