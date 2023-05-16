def RetCC_LVM : CallingConv<[
    CCIfType<[i64], CCAssignToReg<[RAX]>>,
    CCIfType<[i32], CCAssignToReg<[EAX]>>,
    CCIfType<[i16], CCAssignToReg<[AX]>>,
    CCIfType<[i8], CCAssignToReg<[AH]>>,
    CCIfType<[i1, i8, i16, i32, i64], CCAssignToStack<8, 8>>,
    ]>;


def CC_LVM : CallingConv<[
    CCIfType<[i64], CCAssignToReg<[RCX, RDX]>>,
    CCIfType<[i32], CCAssignToReg<[ECX, EDX]>>,
    CCIfType<[i16], CCAssignToReg<[CX, DX]>>,
    CCIfType<[i8], CCAssignToReg<[CH, DH]>>,
    CCIfType<[i1, i8, i16, i32, i64], CCAssignToStack<8, 8>>,
    ]>;

def CC_Save : CalleeSavedRegs<(add RBX)>;