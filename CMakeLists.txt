cmake_minimum_required(VERSION 3.10)
project(adventOfCodeC99)

set(CMAKE_GENERATOR "MinGW Makefiles")
message("generator is set to ${CMAKE_GENERATOR}")

set(CMAKE_C_STANDARD 99)

set(SRCS "2022/src/c99/mainC99.c")
message("files are ${SRCS}")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "2022/bin")
add_executable(mainC99 ${SRCS})