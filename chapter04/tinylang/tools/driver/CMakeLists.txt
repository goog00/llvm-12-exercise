#LLVM_LINK_COMPONENTS变量设置为需要链接的 LLVM 组件列表 (LLVM 组件是 一个或多个库的集合)。
#这取决于工具实现的功能。这里，我们只需要 Support 组件。

set(LLVM_LINK_COMPONENTS 
Support
)

#add_tinylang_tool() 定义了可安装应用程序。名称是 tinylang，唯一的源文件是 Driver.cpp
add_tinylang_tool(tinylang
   Driver.cpp
)

#要链接到自己的库，必须使用 target_link_libraries() 来指定。这里，只需要 tinylangBasic。
target_link_libraries(tinylang
    PRIVATE
    tinylangBasic
)
