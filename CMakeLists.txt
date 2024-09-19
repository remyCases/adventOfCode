cmake_minimum_required(VERSION 3.10)
project(adventOfCodeC99)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

message("generator is set to ${CMAKE_GENERATOR}")

set(CMAKE_C_STANDARD "99")

if(CMAKE_SYSTEM MATCHES Windows)
	message(STATUS "Target system is windows")
	set(CMAKE_C_COMPILER "C:\\msys64\\mingw64\\bin\\gcc.exe")
	add_compile_options("/W4")
else(CMAKE_SYSTEM MATCHES Linux)
	message(STATUS "Target system is linux")
	add_compile_options("-Wall" "-Wextra" "-Werror")
endif()

file(GLOB all_SRCS
    "${YEAR}/src/c99/*.h"
    "${YEAR}/src/c99/*.c"
	"utils/*.h"
	"utils/*.c"
)

message("files are ${all_SRCS}")
cmake_path(APPEND CMAKE_RUNTIME_OUTPUT_DIRECTORY ${YEAR} "bin")
add_executable(mainC99 ${all_SRCS})
