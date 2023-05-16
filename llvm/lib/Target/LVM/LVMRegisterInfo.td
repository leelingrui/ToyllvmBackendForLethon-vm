class LVMReg<string n, bits<8> Enc, list<Register> subregs = []> : Register<n> {
  let Namespace = "LVM";
  let HWEncoding{7-0} = Enc;
  let SubRegs = subregs;
}

let Namespace = "LVM" in {
  def sub_8bit     : SubRegIndex<8>;
  def sub_8bit_hi  : SubRegIndex<8, 8>;
  def sub_8bit_hi_phony  : SubRegIndex<8, 8>;
  def sub_16bit    : SubRegIndex<16>;
  def sub_16bit_hi : SubRegIndex<16, 16>;
  def sub_32bit    : SubRegIndex<32>;
  def sub_xmm      : SubRegIndex<128>;
  def sub_ymm      : SubRegIndex<256>;
  def sub_mask_0   : SubRegIndex<-1>;
  def sub_mask_1   : SubRegIndex<-1, -1>;
}

def AH : LVMReg<"ah", 1>;
def BH : LVMReg<"bh", 2>;
def CH : LVMReg<"ch", 3>;
def DH : LVMReg<"dh", 4>;

let SubRegIndices = [sub_8bit], CoveredBySubRegs = 1 in
{
  def AX : LVMReg<"ax", 1, [AH]>;
  def BX : LVMReg<"bx", 2, [BH]>;
  def CX : LVMReg<"cx", 3, [CH]>;
  def DX : LVMReg<"dx", 4, [DH]>;
}

let SubRegIndices = [sub_16bit], CoveredBySubRegs = 1 in
{
  def EAX : LVMReg<"eax", 1, [AX]>;
  def EBX : LVMReg<"ebx", 2, [BX]>;
  def ECX : LVMReg<"ecx", 3, [CX]>;
  def EDX : LVMReg<"edx", 4, [DX]>;
}
let SubRegIndices = [sub_32bit], CoveredBySubRegs = 1 in {
  def RAX : LVMReg<"rax", 1, [EAX]>;
  def RBX : LVMReg<"rbx", 2, [EBX]>;
  def RCX : LVMReg<"rcx", 3, [ECX]>;
  def RDX : LVMReg<"rdx", 4, [EDX]>;
}

def IP : LVMReg<"ip", 0>;
def RFLAGS : LVMReg<"rflags", 5>;
def RSP : LVMReg<"rsp", 6>;
def RBP : LVMReg<"rbp", 7>;
def RCS : LVMReg<"rcs", 8>;
def RDS : LVMReg<"rds", 9>;
def RES : LVMReg<"res", 10>;
def RFS : LVMReg<"rfs", 11>;
def RGS : LVMReg<"rgs", 12>;





def GR8 : RegisterClass<"LVM", [i8], 8, (add AH, CH, DH, BH)>;
def GR16 : RegisterClass<"LVM", [i16], 16, (add AX, CX, DX, BX)>;
def GR32 : RegisterClass<"LVM", [i32], 32, (add EAX, ECX, EDX, EBX)>;
def GR64 : RegisterClass<"LVM", [i64], 64, (add RAX, RCX, RDX, RBX, RFLAGS, RSP, RBP, RCS, RDS, RES, RFS, RGS)>;