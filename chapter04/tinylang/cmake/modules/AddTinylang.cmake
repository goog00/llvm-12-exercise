## 这些是 LLVM(在 AddLLVM 模块中) 提供的函数包装器。 tinylang subdirectory() 为构建添加了一个新的源目录。
## 此外，还添加了一个新的 CMake 选项。使用此选项，用户可以控制是否应该编译目录的内容。
## 使用 add tinylang library()，可以定义库并安 装。Add tinylang executable() 定义了可执行文件，
## Add tinylang tool() 定义了同样安装的可执行 文件。


macro(add_tinylang_subdirectory name)
  add_llvm_subdirectory(TINYLANG TOOL ${name})
endmacro()

macro(add_tinylang_library name)
  if(BUILD_SHARED_LIBS)
    set(LIBTYPE SHARED)
  else()
    set(LIBTYPE STATIC)
  endif()
  llvm_add_library(${name} ${LIBTYPE} ${ARGN})
  if(TARGET ${name})
    target_link_libraries(${name} INTERFACE ${LLVM_COMMON_LIBS})
    install(TARGETS ${name}
      COMPONENT ${name}
      LIBRARY DESTINATION lib${LLVM_LIBDIR_SUFFIX}
      ARCHIVE DESTINATION lib${LLVM_LIBDIR_SUFFIX}
      RUNTIME DESTINATION bin)
  else()
    add_custom_target(${name})
  endif()
endmacro()

macro(add_tinylang_executable name)
  add_llvm_executable(${name} ${ARGN} )
endmacro()

macro(add_tinylang_tool name)
  add_tinylang_executable(${name} ${ARGN})
  install(TARGETS ${name}
    RUNTIME DESTINATION bin
    COMPONENT ${name})
endmacro()
