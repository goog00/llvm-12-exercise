#ifndef TINYLANG_BASIC_VERSION_H
#define TINYLANG_BASIC_VERSION_H

#include "tinylang/Basic/Version.inc"
#include <string>

// 声明了一个可获取 version 字符串的函数:
namespace tinylang {
    std::string getTinylangVersion();
}
#endif