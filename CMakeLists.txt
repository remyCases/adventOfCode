cmake_minimum_required(VERSION 3.10)

project(adventOfCodeC99)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

message(STATUS "Generator is set to ${CMAKE_GENERATOR}")

set(CMAKE_C_STANDARD "99")

if(CMAKE_SYSTEM MATCHES Windows)
	message(STATUS "Target system is windows")
else(CMAKE_SYSTEM MATCHES Linux)
	message(STATUS "Target system is linux")
endif()

if(MSVC)
    # MSVC-specific flags
    add_compile_options(
        /W4     			# Warning level 4
        /MP     			# Multi-processor compilation
        /Zi     			# Debug information format
    )
else()
    # GCC/Clang flags
    add_compile_options(
        -Wall
        -Wextra
		-Werror
        -Wpedantic  		# Strict ISO C/C++ but can be used with -Werror
		-O2                 # Optimization level 2 (good balance of speed vs size)
        -pthread    		# Threading support
		-fstack-protector   # Stack protection against buffer overflows
    )
    
    # Optional: Specific GCC vs Clang flags
    if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
        add_compile_options(-fdiagnostics-color=always)
    elseif(CMAKE_C_COMPILER_ID MATCHES "Clang")
        add_compile_options(-fcolor-diagnostics)
    endif()
endif()

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    if(MSVC)
        add_compile_options(/Od /RTC1)  # Disable optimization, enable runtime checks
    else()
        add_compile_options(-O0 -g3)    # No optimization, max debug info
    endif()
endif()

file(GLOB all_SRCS
    "${YEAR}/src/c99/*.h"
    "${YEAR}/src/c99/*.c"
	"utils/*.h"
	"utils/*.c"
)

message(STATUS "Files are ${all_SRCS}")
cmake_path(APPEND CMAKE_RUNTIME_OUTPUT_DIRECTORY "${YEAR}" "bin")
add_executable(mainC99 ${all_SRCS})
