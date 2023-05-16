add_llvm_component_library(LLVMLVMDisassembler
    LVMDisassembler.cpp

    LINK_COMPONENTS
    MC

    ADD_TO_COMPONENT
    LVM
)