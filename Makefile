# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

BINS := $(wildcard build/*/bin/main*)
BINS += $(wildcard build/rust/*)
BINS += $(wildcard build/CMakeCache.txt)

# Detect OS
ifeq ($(OS), Windows_NT)
    DETECTED_OS := Windows
else
    DETECTED_OS := $(shell uname -s)
endif

# executables needed
EXECUTABLES = nim cargo $(LUA) luarocks luacheck cmake ninja gcc cobc zig as $(PYTHON)

# Default values
BUILD_TYPE ?= Release

# C99 specific default values
COMPILER ?= gcc
VERBOSE ?= OFF
CMAKE_GENERATOR ?= "Ninja"

# Build tracking
BUILD_LOG := build/build.log
SUCCESS_LOG := build/success.log
FAILURE_LOG := build/failure.log

# Cross compatibilty Linux-Windows
ifeq ($(DETECTED_OS), Linux)
	RM = rm -rf
	RMBINS = $(BINS)
	RMLOGS = $(BUILD_LOG) $(SUCCESS_LOG) $(FAILURE_LOG)
	MD = mkdir -p
	CAT = cat
	CP = cp
	SANITIZE ?= OFF
	ZIGBUILDDIR = .
	WHERE = which
	LUA = lua
	PYTHON = python3
	VENV=venv
	BIN=$(VENV)/bin
	PIP=$(BIN)/pip
	VENV_MYPY=$(BIN)/$(MYPY)
	VENV_PYTHON=$(BIN)/$(PYTHON)
	SHELL_EXEC = /bin/bash
	ECHO = echo
	NULL_DEVICE = /dev/null
	CONTINUE_ON_ERROR = || true
	GET = 
	LINE_COUNT = wc --lines
else
	RM = powershell -Command "Remove-Item -Recurse"
	RMBINS = $(subst $(space),$(comma),$(BINS))
	RMLOGS = $(subst $(space),$(comma),$(BUILD_LOG) $(SUCCESS_LOG) $(FAILURE_LOG))
	MD = powershell -Command "New-Item -Type Directory"
	CAT = powershell -Command "Get-Content -encoding UTF8"
	CP = powershell -Command "Copy-Item"
	COBOL_CONFIG_FLAG ?= -conf ./utils/default.conf
	EXTRA_LIB ?= -LC:/msys64/mingw32/lib -lkernel32
	ZIGBUILDDIR = $(CWD)
	WHERE = where.exe
	LUA = lua54
	PYTHON = python
	VENV=.venv
	BIN=$(VENV)\Scripts
	PIP=$(BIN)\pip
	VENV_MYPY=$(BIN)\$(MYPY)
	VENV_PYTHON=$(BIN)\$(PYTHON)
	SHELL_EXEC = powershell
	ECHO = powershell -Command "Write-Host"
	NULL_DEVICE = NUL
	CONTINUE_ON_ERROR = ; exit 0
	GET = powershell Get-Content
	LINE_COUNT = Measure-Object -Line | Select -Expand Lines
endif
REDIRECTION = 2>$(NULL_DEVICE)
MYPY=mypy

# Files and folders
NIMFOLDER := $(wildcard */src/nim/mainNim.nim)
NIM_TARGETS := $(NIMFOLDER:%/src/nim/mainNim.nim=nim_%)

C99FOLDER := $(wildcard */src/c99/mainC99.c)
C99_TARGETS := $(C99FOLDER:%/src/c99/mainC99.c=c99_%)

COBFOLDER := $(wildcard */src/cobol/mainCob.cob)
COB_TARGETS := $(COBFOLDER:%/src/cobol/mainCob.cob=cob_%)

CARGOFACTORY_FILE := ./buildtools/CargoFactory.toml
CARGO_FILE := ./Cargo.toml
RUSTFOLDER := $(wildcard */src/rust/main_rust.rs)
CARGO_TARGETS := $(RUSTFOLDER:%/src/rust/main_rust.rs=%)
COPY_RUST_TARGETS := $(CARGO_TARGETS:%=copy_rust_%)

ZIGFOLDER := $(wildcard */src/zig/build.zig)
ZIG_TARGETS := $(ZIGFOLDER:%/src/zig/build.zig=zig_%)

PYFOLDER := $(wildcard */src/py/main_py.py)
PY_TARGETS := $(PYFOLDER:%/src/py/main_py.py=py_%)

LUAFOLDER := $(wildcard */src/lua/mainLua.lua)
LUA_TARGETS := $(LUAFOLDER:%/src/lua/mainLua.lua=lua_%)

ASMFOLDER := $(wildcard */src/asm/mainAsm.s)
ASM_TARGETS := $(ASMFOLDER:%/src/asm/mainAsm.s=asm_%)

FOLDERS := $(wildcard 20*)
YEARFOLDERS := $(FOLDERS:%=folder_%)

# Convenient values
empty:=
space:= $(empty) $(empty)
comma:= ,

.DELETE_ON_ERROR:
build_all: prerequisite build_nim build_rust build_c99 build_cob build_zig build_py build_lua build_summary

clean_logs:
	@$(RM) $(RMLOGS) $(REDIRECTION) $(CONTINUE_ON_ERROR)
	@$(MD) build $(REDIRECTION) $(CONTINUE_ON_ERROR)

build_summary:
	@$(LUA) ./buildtools/BuildSummary.lua

prerequisite_exe:
	$(info Checking prerequisites...)
	@$(foreach exec, $(EXECUTABLES), \
		$(if $(shell $(WHERE) $(exec) $(REDIRECTION)), \
			$(info Found $(exec)), $(error "No $(exec) in PATH")))

prerequisite_folders: $(YEARFOLDERS)
prerequisite: clean_logs prerequisite_exe prerequisite_folders
folder_%:
	@$(MD) build/$*/bin  $(REDIRECTION) $(CONTINUE_ON_ERROR)

### NIM ###
build_nim: prerequisite $(NIM_TARGETS)
build_nim_debug: 
	$(MAKE) build_nim BUILD_TYPE=Debug
nim_%:
	@$(ECHO) "Building Nim for $*..."
ifeq ($(BUILD_TYPE), Release)
	@nim c -o=build/$*/bin/mainNim -d=release --nimcache=build/$*/nimcache --hints=off ./$*/src/nim/mainNim.nim 2>&1 && \
	$(ECHO) "nim_$*" >> $(SUCCESS_LOG) || \
	$(ECHO) "nim_$*" >> $(FAILURE_LOG) \
	$(REDIRECTION) $(CONTINUE_ON_ERROR)
else
ifeq ($(BUILD_TYPE), Debug)
	@nim c -o=build/$*/bin/mainNim -d=debug --nimcache=build/$*/nimcache --hints=on ./$*/src/nim/mainNim.nim 2>&1 && \
	$(ECHO) "nim_$*" >> $(SUCCESS_LOG) || \
	$(ECHO) "nim_$*" >> $(FAILURE_LOG) \
	$(REDIRECTION) $(CONTINUE_ON_ERROR)
endif
endif


### RUST ###
clippy: build_cargo
	cargo clippy --profile $(BUILD_TYPE) --manifest-path $(CARGO_FILE) --target-dir ./build/rust

test_rust: build_rust
	cargo test --profile $(BUILD_TYPE) --manifest-path $(CARGO_FILE) --target-dir ./build/rust

build_rust: prerequisite build_cargo rust_target $(COPY_RUST_TARGETS)

.PHONY: build_rust_debug build_rust_release
build_rust_debug:
	$(MAKE) build_rust BUILD_TYPE=Dev

build_rust_release:
	$(MAKE) build_rust BUILD_TYPE=Release

build_cargo:
	@$(LUA) ./buildtools/CargoFactory.lua $(CARGO_TARGETS)

rust_target:
	@$(ECHO) "Building Rust targets..."
	@cargo build --profile $(BUILD_TYPE) --manifest-path $(CARGO_FILE) --target-dir ./build/rust 2>&1 && \
	$(ECHO) "rust_common" >> $(SUCCESS_LOG) || \
	$(ECHO) "rust_common" >> $(FAILURE_LOG) \
	$(REDIRECTION) $(CONTINUE_ON_ERROR)

copy_rust_%:
ifeq ($(DETECTED_OS), Windows)
	@$(CP) ./build/rust/$(BUILD_TYPE)/main_rust$*.exe -Destination build/$*/bin/mainRust.exe \
	$(REDIRECTION) $(CONTINUE_ON_ERROR)
else
	@$(CP) ./build/rust/$(BUILD_TYPE)/main_rust$* build/$*/bin/mainRust \
	$(REDIRECTION) $(CONTINUE_ON_ERROR)
endif

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
	@$(ECHO) "Building C99 for $*..."
	@$(CMAKE_CMD) 1>$(NULL_DEVICE) 2>&1 && \
	cd build && ninja && \
	$(ECHO) "c99_$*" >> ../$(SUCCESS_LOG) || \
	$(ECHO) "c99_$*" >> ../$(FAILURE_LOG) \
	$(REDIRECTION) $(CONTINUE_ON_ERROR)

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
build_cob_debug: 
	$(MAKE) build_cob BUILD_TYPE=Debug
cob_%:
	@$(ECHO) "Building gnucobol for $*..."
ifeq ($(BUILD_TYPE), Release)
	@cobc -x -free -o build/$*/bin/mainCob $*/src/cobol/mainCob.cob $(wildcard $*/src/cobol/day*.cob) $(COBOL_CONFIG_FLAG) 2>&1 && \
	$(ECHO) "cob_$*" >> $(SUCCESS_LOG) || \
	$(ECHO) "cob_$*" >> $(FAILURE_LOG) \
	$(REDIRECTION) $(CONTINUE_ON_ERROR)
else
ifeq ($(BUILD_TYPE), Debug)
	@cobc -x -free -g --debug -o build/$*/bin/mainCob $*/src/cobol/mainCob.cob $(wildcard $*/src/cobol/day*.cob) $(COBOL_CONFIG_FLAG) 2>&1 && \
	$(ECHO) "cob_$*" >> $(SUCCESS_LOG) || \
	$(ECHO) "cob_$*" >> $(FAILURE_LOG) \
	$(REDIRECTION) $(CONTINUE_ON_ERROR)
endif
endif

### ZIG ###
build_zig: prerequisite $(ZIG_TARGETS)
build_zig_debug: 
	$(MAKE) build_zig BUILD_TYPE=Debug
zig_%:
	@$(ECHO) "Building zig for $*..."
ifeq ($(BUILD_TYPE), Release)
	@zig build -Doptimize=ReleaseSmall -p ./build/$* --build-file ./$*/src/zig/build.zig --cache-dir $(ZIGBUILDDIR)/build/$*/.zig-cache --summary all && \
	$(ECHO) "zig_$*" >> $(SUCCESS_LOG) || \
	$(ECHO) "zig_$*" >> $(FAILURE_LOG) \
	$(REDIRECTION) $(CONTINUE_ON_ERROR)
else
ifeq ($(BUILD_TYPE), Debug)
	@zig build -Doptimize=Debug -p ./build/$* --build-file ./$*/src/zig/build.zig --cache-dir $(ZIGBUILDDIR)/build/$*/.zig-cache --summary all 1>$(NULL_DEVICE) 2>&1 && \
	$(ECHO) "zig_$*" >> $(SUCCESS_LOG) || \
	$(ECHO) "zig_$*" >> $(FAILURE_LOG) \
	$(REDIRECTION) $(CONTINUE_ON_ERROR)
endif
endif

### PYTHON ###
build_py: prerequisite $(PY_TARGETS)
py_%:
	@$(ECHO) "Building python for $*..."
	@$(PYTHON) -m venv ./$*/src/py/$(VENV)
	@./$*/src/py/$(VENV_PYTHON) -m pip install --quiet --upgrade pip
	@$(LUA) ./buildtools/RequirementsFactory.lua ./$*/src/py/requirements.txt
	@./$*/src/py/$(PIP) install --quiet --upgrade -r ./$*/src/py/requirements.txt
	@./$*/src/py/$(VENV_MYPY) . --strict
ifeq ($(DETECTED_OS), Linux)
	@echo > build/$*/bin/mainPy
	@echo #!/bin/bash > build/$*/bin/mainPy
	@echo ./$*/src/py/$(VENV_PYTHON) ./$*/src/py/main_py.py \$$@ >> build/$*/bin/mainPy
	@chmod +x build/$*/bin/mainPy
else
	@echo @echo off > build/$*/bin/mainPy.bat
	@echo .\$*\src\py\$(VENV_PYTHON) .\$*\src\py\main_py.py %%* >> build\$*\bin\mainPy.bat
endif

### LUA ###
build_lua: prerequisite $(LUA_TARGETS)
lua_%:
	@$(ECHO) "Building lua for $*..."
	@luacheck ./$*/src/lua
ifeq ($(DETECTED_OS), Linux)
	@echo > build/$*/bin/mainLua
	@echo #!/bin/bash > build/$*/bin/mainLua
	@echo $(LUA) ./$*/src/lua/mainLua.lua \$$@ >> build/$*/bin/mainLua
	@chmod +x build/$*/bin/mainLua
else
	@echo @echo off > build/$*/bin/mainLua.bat
	@echo $(LUA) .\$*\src\lua\mainLua.lua %%* >> build\$*\bin\mainLua.bat
endif

### ASM ###
build_asm: prerequisite $(ASM_TARGETS)
asm_%:
	@$(ECHO) "Building asm for $*..."
	@as ./$*/src/asm/mainAsm.s --32 -gstabs -Wa --defsym $(DETECTED_OS)=1 -o ./build/$*/bin/mainAsm.o 1>$(NULL_DEVICE) 2>&1 && \
	as ./utils/print.s --32 -gstabs -Wa --defsym $(DETECTED_OS)=1 -o ./build/$*/bin/print.o 1>$(NULL_DEVICE) 2>&1 && \
	as ./$*/src/asm/dayOne.s --32 -gstabs -Wa -o ./build/$*/bin/dayOne.o 1>$(NULL_DEVICE) 2>&1 && \
	gcc -o ./build/$*/bin/mainAsm -ggdb -m32 ./build/$*/bin/mainAsm.o ./build/$*/bin/print.o -nostdlib -no-pie $(EXTRA_LIB) && \
	$(ECHO) "asm_$*" >> $(SUCCESS_LOG) || \
	$(ECHO) "asm_$*" >> $(FAILURE_LOG) \
	$(REDIRECTION) $(CONTINUE_ON_ERROR)

### EMULATION = elf_i386 for linux
### EMULATION = i386pe for windows
### ld -o ./build/$*/bin/mainAsm -m $(EMULATION) -LC:/msys64/mingw32/lib -lkernel32 ./build/$*/bin/mainAsm.o

### CLEAN ###
clean:
	@$(RM) $(RMBINS) $(REDIRECTION) $(CONTINUE_ON_ERROR)

### RESET ###
reset:
	@$(RM) ./build  $(REDIRECTION) $(CONTINUE_ON_ERROR)
