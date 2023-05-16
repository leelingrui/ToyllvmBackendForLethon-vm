add_llvm_component_library(LLVMLVMInfo
    LVMTargetInfo.cpp

    LINK_COMPONENTS
    MC

    ADD_TO_COMPONENT
    LVM
)