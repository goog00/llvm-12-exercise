## 定义了一个名为 tinylangBasic 的新库，并将编译后的 Version.cpp 添加到这个库中。
## LLVM 选 项可以控制这是一个动态库还是静态库。默认情况下，会创建静态库

add_tinylang_library(tinylangBasic 
    Version.cpp
)