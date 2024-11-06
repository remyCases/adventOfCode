# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

BINS := $(wildcard build/*/bin/main*)
BINS += $(wildcard build/rust/release/main_rust*)
BINS += $(wildcard build/CMakeCache.txt)
$(info $(BINS))

# Detect OS
ifeq ($(OS), Windows_NT)
    DETECTED_OS := Windows
else
    DETECTED_OS := $(shell uname -s)
endif

# Default values
BUILD_TYPE ?= Release

# C99 specific default values
COMPILER ?= gcc
VERBOSE ?= OFF
CMAKE_GENERATOR ?= "Ninja"

# Cross compatibilty Linux-Windows
ifeq ($(DETECTED_OS), Linux)
	RM = rm -f
	RMBINS = $(BINS)
	MD = mkdir -p
	CAT = cat
	COBOL_CONFIG_FILE = /usr/local/share/gnucobol/config/default.conf 
else
	RM = powershell Remove-Item -Path
	RMBINS = $(subst $(space),$(comma),$(BINS))
	MD = powershell New-Item -Type Directory -Force
	CAT = powershell Get-Content -encoding UTF8
	COBOL_CONFIG_FILE = ./utils/default.conf
	SANITIZE ?= OFF
endif

# Files and folders
NIMFOLDER := $(wildcard */src/nim/mainNim.nim)
NIM_TARGETS := $(NIMFOLDER:%/src/nim/mainNim.nim=nim_%)

C99FOLDER := $(wildcard */src/c99/mainC99.c)
C99_TARGETS := $(C99FOLDER:%/src/c99/mainC99.c=c99_%)

COBFOLDER := $(wildcard */src/cobol/mainCob.cob)
COB_TARGETS := $(COBFOLDER:%/src/cobol/mainCob.cob=cob_%)

CARGOFACTORY_FILE := ./utils/CargoFactory.toml
CARGO_FILE := ./utils/Cargo.toml
RUSTFOLDER := $(wildcard */src/rust/main_rust.rs)
CARGO_TARGETS := $(RUSTFOLDER:%/src/rust/main_rust.rs=%)

ZIGFOLDER := $(wildcard */src/zig/build.zig)
ZIG_TARGETS := $(ZIGFOLDER:%/src/zig/build.zig=zig_%)

ASMFOLDER := $(wildcard */src/asm/mainAsm.s)
ASM_TARGETS := $(ASMFOLDER:%/src/asm/mainAsm.s=asm_%)

FOLDERS := $(wildcard 20*)
YEARFOLDERS := $(FOLDERS:%=folder_%)

# Convenient values
empty:=
space:= $(empty) $(empty)
comma:= ,

.DELETE_ON_ERROR:
build_all: prerequisite build_nim build_rust build_c99 build_cob build_zig build_asm

prerequisite: $(YEARFOLDERS)
folder_%:
	$(MD) build/$*/bin

### NIM ###
build_nim: prerequisite $(NIM_TARGETS)
nim_%: 
	nim c -o=build/$*/bin/mainNim -d=release --nimcache=build/$*/nimcache --hints=on ./$*/src/nim/mainNim.nim

### RUST ###
clippy: build_cargo
	cargo clippy --manifest-path $(CARGO_FILE)

build_rust: prerequisite build_cargo rust_target

.PHONY: build_rust_debug build_rust_release
build_rust_debug:
	$(MAKE) build_rust BUILD_TYPE=Dev

build_rust_release:
	$(MAKE) build_rust BUILD_TYPE=Release

build_cargo:
ifeq ($(DETECTED_OS), Windows)
	lua54 .\utils\CargoFactory.lua $(CARGO_TARGETS)
else
	lua .\utils\CargoFactory.lua $(CARGO_TARGETS)
endif

rust_target:
	cargo build --profile $(BUILD_TYPE) --manifest-path $(CARGO_FILE) --target-dir ./build/rust

### C99 ###
build_c99: prerequisite $(C99_TARGETS)

# Base build command
CMAKE_BASE_CMD = cmake -Bbuild -G $(CMAKE_GENERATOR) \
    -DYEAR=$* \
    -DCMAKE_BUILD_TYPE=$(BUILD_TYPE) \
    -DCMAKE_C_COMPILER=$(COMPILER) \
    -DCMAKE_VERBOSE_MAKEFILE=$(VERBOSE)

# OS-specific build commands
ifeq ($(DETECTED_OS),Windows)
    CMAKE_CMD = $(CMAKE_BASE_CMD)
else
    CMAKE_CMD = $(CMAKE_BASE_CMD) -DENABLE_SANITIZER=$(SANITIZE)
endif

c99_%:
	$(CMAKE_CMD)
	cd build && ninja

# Common targets for both OS
.PHONY: build_c99_debug build_c99_release build_c99_verbose
build_c99_debug:
	$(MAKE) build_c99 BUILD_TYPE=Debug

build_c99_release:
	$(MAKE) build_c99 BUILD_TYPE=Release

build_c99_verbose:
	$(MAKE) build_c99 VERBOSE=ON

# Unix-only targets
ifneq ($(DETECTED_OS),Windows)
.PHONY: build_c99_asan build_c99_msan
build_c99_asan:
	$(MAKE) build_c99 BUILD_TYPE=Debug SANITIZE=address

build_c99_msan:
	$(MAKE) build_c99 BUILD_TYPE=Debug SANITIZE=memory
endif

### COBOL ###
build_cob: prerequisite $(COB_TARGETS)
cob_%:
	cobc -x -free -o build/$*/bin/mainCob $*/src/cobol/mainCob.cob $(wildcard $*/src/cobol/day*.cob) -conf $(COBOL_CONFIG_FILE)

### ZIG ###
build_zig: prerequisite $(ZIG_TARGETS)
zig_%:
	zig build -Doptimize=ReleaseSmall -p ./build/$* --build-file ./$*/src/zig/build.zig --cache-dir ./build/$*/zig-cache

### ASM ###
build_asm: prerequisite $(ASM_TARGETS)
asm_%:
	as ./$*/src/asm/mainAsm.s --32 -g -o ./build/$*/bin/mainAsm.o
	gcc -o ./build/$*/bin/mainAsm -m32 ./build/$*/bin/mainAsm.o -nostdlib -no-pie

### CLEAN ###
clean:
	$(RM) $(RMBINS)
