cmake_minimum_required(VERSION 3.10)
project(adventOfCodeC99)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

message("generator is set to ${CMAKE_GENERATOR}")

set(CMAKE_C_STANDARD "99")
set(CMAKE_C_COMPILER "C:\\msys64\\mingw64\\bin\\gcc.exe")
set(CMAKE_BUILD_TYPE "Debug")

file(GLOB all_SRCS
        "${YEAR}/src/c99/*.h"
        "${YEAR}/src/c99/*.c"
        )

message("files are ${all_SRCS}")
cmake_path(APPEND CMAKE_RUNTIME_OUTPUT_DIRECTORY ${YEAR} "bin")
add_executable(mainC99 ${all_SRCS})