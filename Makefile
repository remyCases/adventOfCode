# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

CARGOFACTORY_FILE = ./utils/CargoFactory.toml
CARGO_FILE = ./utils/Cargo.toml
CONFIG_FILE = ./utils/default.conf

NIMFOLDER := $(wildcard */src/nim/mainNim.nim)
NIM_TARGETS := $(NIMFOLDER:%/src/nim/mainNim.nim=nim_%)

C99FOLDER := $(wildcard */src/c99/mainC99.c)
C99_TARGETS := $(C99FOLDER:%/src/c99/mainC99.c=c99_%)

COBFOLDER := $(wildcard */src/cobol/mainCob.cob)
COB_TARGETS := $(COBFOLDER:%/src/cobol/mainCob.cob=cob_%)

RUSTFOLDER := $(wildcard */src/rust/main_rust.rs)
CARGO_TARGETS := $(RUSTFOLDER:%/src/rust/main_rust.rs=cargo_%)

BINS := $(wildcard build/*/bin/*.exe)
LIBS := $(wildcard build/*/lib/*.dll)
empty:=
space:= $(empty) $(empty)
comma:= ,

.DELETE_ON_ERROR:
build_all: build_nim build_rust build_c99 build_cob

build_nim: $(NIM_TARGETS)
nim_%: 
	nim c -o=build/$*/bin/mainNim -d=release --nimcache=build/$*/nimcache --hints=on ./$*/src/nim/mainNim.nim

clippy: build_cargo
	cargo clippy --manifest-path $(CARGO_FILE)

build_rust: build_cargo rust_target
build_rust_debug: build_cargo rust_target_debug
build_cargo: header_cargo $(CARGO_TARGETS)

header_cargo:
	powershell type $(CARGOFACTORY_FILE) > $(CARGO_FILE)

cargo_%: 
	@echo [[bin]] >> $(CARGO_FILE)
	@echo name="main_rust$*" >> $(CARGO_FILE)
	@echo path="../$*/src/rust/main_rust.rs" >> $(CARGO_FILE)

rust_target:
	cargo build --release --manifest-path $(CARGO_FILE) --target-dir ./build/rust/bin
	
rust_target_debug:
	cargo build --manifest-path $(CARGO_FILE) --target-dir ./build/rust/bin

build_c99: $(C99_TARGETS)
	cd build && make all

c99_%:
	cmake -Bbuild -G "MinGW Makefiles" -DYEAR=$*

build_cob: $(COB_TARGETS)
.SECONDEXPANSION:
cob_%: $$(wildcard $$*/src/cobol/day*.cob)
	cobc -x -free -o build/$*/bin/mainCob -conf $(CONFIG_FILE) $*/src/cobol/mainCob.cob

%.cob:
	cobc -m -o build/$(firstword $(subst /, ,$*))/lib/$(lastword $(subst /, ,$*)).dll -conf $(CONFIG_FILE) $@


clean:
	powershell Remove-Item -Path $(subst $(space),$(comma),$(BINS))
	powershell Remove-Item -Path $(subst $(space),$(comma),$(LIBS))