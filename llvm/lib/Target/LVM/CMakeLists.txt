add_llvm_component_group(LVM)


set(LLVM_TARGET_DEFINITIONS LVM.td)
tablegen(LLVM LVMGenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM LVMGenMCPseudoLowering.inc -gen-pseudo-lowering)
tablegen(LLVM LVMGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM LVMGenCallingConv.inc -gen-callingconv)
tablegen(LLVM LVMGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM LVMGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM LVMGenMCPseudoLowering.inc -gen-pseudo-lowering)
tablegen(LLVM LVMGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM LVMGenSubtargetInfo.inc -gen-subtarget)
tablegen(LLVM LVMGenDisassemblerTables.inc -gen-disassembler)
tablegen(LLVM LVMGenMCCodeEmitter.inc -gen-emitter)

add_public_tablegen_target(LVMCommonTableGen)

set(sources
  LVMTargetMachine.cpp
  LVMISelDAGToDAG.cpp
  LVMAsmPrinter.cpp
  LVMSubtarget.cpp
  LVMISelLowering.cpp
  LVMFrameLowering.cpp
  LVMInstrInfo.cpp
  LVMRegisterInfo.cpp
  LVMMCInstLower.cpp
  LVMMachineFunctionInfo.cpp
  LVMAnalyzeImmediate.cpp
  LVMTargetObjectFile.cpp
)

add_llvm_target(LVMCodeGen ${sources}
  LINK_COMPONENTS
  AsmPrinter
  Core
  CodeGen
  MC
  LVMDisassembler
  LVMDesc
  LVMInfo
  LVMAsmParser
  SelectionDAG
  Support
  Target

  ADD_TO_COMPONENT
  LVM
)

add_subdirectory(MCTargetDesc)
add_subdirectory(TargetInfo)
add_subdirectory(Disassembler)
add_subdirectory(AsmParser)