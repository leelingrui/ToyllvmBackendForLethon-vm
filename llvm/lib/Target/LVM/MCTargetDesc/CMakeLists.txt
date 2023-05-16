add_llvm_component_library(LLVMLVMDesc
    LVMMCTargetDesc.cpp
    LVMMCAsmInfo.cpp
    LVMABIInfo.cpp
    LVMMCExpr.cpp
    LVMTargetStreamer.cpp
    LVMELFObjectWriter.cpp
    LVMAsmBackend.cpp
    LVMMCCodeEmitter.cpp
    LVMELFStreamer.cpp
    LVMInstPrinter.cpp

    LINK_COMPONENTS
    MC
    LVMInfo
    Support

    ADD_TO_COMPONENT
    LVM
)