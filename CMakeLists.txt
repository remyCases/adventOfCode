cmake_minimum_required(VERSION 3.10)
project(adventOfCodeC99)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_GENERATOR "MinGW Makefiles")
message("generator is set to ${CMAKE_GENERATOR}")

set(CMAKE_C_STANDARD "99")
set(CMAKE_C_COMPILER "D:\\msys64\\mingw64\\bin\\gcc.exe")
set(CMAKE_BUILD_TYPE "Debug")

file(GLOB all_SRCS
        "2022/src/c99/*.h"
        "2022/src/c99/*.c"
        )

message("files are ${all_SRCS}")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "2022/bin")
add_executable(mainC99 ${all_SRCS})