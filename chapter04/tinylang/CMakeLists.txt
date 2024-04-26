cmake_minimum_required(VERSION 3.13.4)
if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
    project(Tinylang)
     
    #搜索LLVM包，将 LLVM 目录添加到 CMake 模块路径中
    find_package(LLVM REQUIRED HINTS "${LLVM_CMAKE_PATH}")
    list(APPEND CMAKE_MODULE_PATH ${LLVM_DIR})

    #使用 Visual Studio 作为构建编译器，并设置正确的运行时库，再次链接时才需要用到
    #include(ChooseMSVCCRT) 
    #添加 LLVM 使用的宏
    include(AddLLVM) 
    include(HandleLLVMOptions)

    #将LLVM头文件的路径添加到include搜索路径中。
    #新增两个目录。添加了构建目录中的include目录，另一个include目录位于源目录中
    include_directories("${LLVM_BINARY_DIR}/include" "${LLVM_INCLUDE_DIR}")
    #LLVM 库的路径添加到链接器
    link_directories("${LLVM_LIBRARY_DIR}")

    #设置一个标志来表示项目是独立构建
    set(TINYLANG_BUILT_STANDALONE 1)

endif()

list(APPEND CMAKE_MODULE_PATH  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR AND NOT MSVC_IDE)
   message(FATAL_ERROR "In-source builds are not allowed")
endif()

#设置tinylang 版本号
set(TINYLANG_VERSION_STRING "O.1")
#tinylang 的版本号通过 configure file() 命令写到生成文件中
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/include/tinylang/Basic/Version.inc.in
    ${CMAKE_CURRENT_BINARY_DIR}/include/tinylang/Basic/Version.inc)

#包含另一个 CMake 模块
include(AddTinylang)


#我们自己的include目录添加到搜索路径的开头
include_directories(BEFORE
   ${CMAKE_CURRENT_BINARY_DIR}/include
   ${CMAKE_CURRENT_SOURCE_DIR}/include
)

#在 lib 和 tools 目录其中找到 CMakeLists.txt
add_subdirectory(lib)
add_subdirectory(tools)