# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

UNAME := $(shell uname)
CARGOFACTORY_FILE = ./utils/CargoFactory.toml
CARGO_FILE = ./utils/Cargo.toml

BINS := $(wildcard build/*/bin/main*)

ifeq ($(UNAME), Linux)
	RM = rm -f
	RMBINS = $(BINS)
	CONFIG_FILE = /usr/local/share/gnucobol/config/default.conf 
else
	RM = powershell Remove-Item -Path
	RMBINS = $(subst $(space),$(comma),$(BINS))
	CONFIG_FILE = ./utils/default.conf
endif

NIMFOLDER := $(wildcard */src/nim/mainNim.nim)
NIM_TARGETS := $(NIMFOLDER:%/src/nim/mainNim.nim=nim_%)

C99FOLDER := $(wildcard */src/c99/mainC99.c)
C99_TARGETS := $(C99FOLDER:%/src/c99/mainC99.c=c99_%)
C99_DEBUG_TARGETS := $(C99FOLDER:%/src/c99/mainC99.c=c99_debug_%)

COBFOLDER := $(wildcard */src/cobol/mainCob.cob)
COB_TARGETS := $(COBFOLDER:%/src/cobol/mainCob.cob=cob_%)

RUSTFOLDER := $(wildcard */src/rust/main_rust.rs)
CARGO_TARGETS := $(RUSTFOLDER:%/src/rust/main_rust.rs=cargo_%)

ZIGFOLDER := $(wildcard */src/zig/mainZig.zig)
ZIG_TARGETS := $(ZIGFOLDER:%/src/zig/mainZig.zig=zig_%)

ASMFOLDER := $(wildcard */src/asm/mainAsm.s)
ASM_TARGETS := $(ASMFOLDER:%/src/asm/mainAsm.s=asm_%)

FOLDERS := $(wildcard 20*)
YEARFOLDERS := $(FOLDERS:%=folder_%)

empty:=
space:= $(empty) $(empty)
comma:= ,

.DELETE_ON_ERROR:
build_all: prerequisite build_nim build_rust build_c99 build_cob build_zig

prerequisite: $(YEARFOLDERS)
folder_%:
	mkdir -p build/$*/bin

build_nim: prerequisite $(NIM_TARGETS)
nim_%: 
	nim c -o=build/$*/bin/mainNim -d=release --nimcache=build/$*/nimcache --hints=on ./$*/src/nim/mainNim.nim

clippy: build_cargo
	cargo clippy --manifest-path $(CARGO_FILE)

build_rust: prerequisite build_cargo rust_target
build_rust_debug: prerequisite build_cargo rust_target_debug
build_cargo: header_cargo $(CARGO_TARGETS)

header_cargo:
	cat $(CARGOFACTORY_FILE) > $(CARGO_FILE)

cargo_%:
	@printf "\n[[bin]]\nname=\"main_rust$*\"\npath=\"../$*/src/rust/main_rust.rs\"" >> $(CARGO_FILE)

rust_target:
	cargo build --release --manifest-path $(CARGO_FILE) --target-dir ./build/rust/bin

rust_target_debug:
	cargo build --manifest-path $(CARGO_FILE) --target-dir ./build/rust/bin

build_c99: prerequisite $(C99_TARGETS)

build_c99_debug: prerequisite $(C99_DEBUG_TARGETS)

c99_%:
	cmake -Bbuild -G "Ninja" -DYEAR=$* -DCMAKE_BUILD_TYPE=Release
	cd build && ninja

c99_debug_%:
	cmake -Bbuild -G "Ninja" -DYEAR=$* -DCMAKE_BUILD_TYPE=Debug
	cd build && ninja

build_cob: prerequisite $(COB_TARGETS)
cob_%:
	cobc -x -free -o build/$*/bin/mainCob $*/src/cobol/mainCob.cob $(wildcard $*/src/cobol/day*.cob)

build_zig: prerequisite $(ZIG_TARGETS)
zig_%:
	zig build-exe -O ReleaseSmall -femit-bin=./build/$*/bin/mainZig ./$*/src/zig/mainZig.zig

build_asm: prerequisite $(ASM_TARGETS)
asm_%:
	as ./$*/src/asm/mainAsm.s --32 -g -o ./build/$*/bin/mainAsm.o
	gcc -o ./build/$*/bin/mainAsm -m32 ./build/$*/bin/mainAsm.o -nostdlib -no-pie

clean:
	$(RM) $(RMBINS)
