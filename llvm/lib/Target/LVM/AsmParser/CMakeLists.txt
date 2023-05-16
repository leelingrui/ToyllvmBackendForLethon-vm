add_llvm_component_library(LLVMLVMAsmParser
    LVMAsmParser.cpp

    LINK_COMPONENTS
    MC

    ADD_TO_COMPONENT
    LVM
)