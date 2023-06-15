# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

SRC = 2022/src
BIN = build/2022/bin
LIB = build/2022/lib
CARGO_FILE = 2022/src/rust/Cargo.toml
BINS = $(wildcard $(BIN)/*.exe)
DLLS = $(wildcard $(LIB)/*.dll)

NIMFLAGS = -o=$(BIN) -d=release --nimcache=build/2022/nimcache --hints=on

.DELETE_ON_ERROR:
build_all: build_nim build_rust build_c99 build_cob

build_nim:
	nim c $(NIMFLAGS) ./2022/src/nim/mainNim.nim

build_rust:
	cargo build --release --manifest-path $(CARGO_FILE) --target-dir $(BIN)

build_c99:
	cmake -Bbuild -G "MinGW Makefiles"
	cd build && make all

COBOL_SRC := dayOne.cob dayTwo.cob dayThree.cob dayFour.cob dayFive.cob
COBOL_DLL := $(COBOL_SRC:.cob=.dll)

build_cob: $(COBOL_DLL)
	cobc -x -free -o $(BIN)/mainCob -conf=./utils/default.conf $(SRC)/cobol/mainCob.cob

%.dll: %.cob
	cobc -m -o $(LIB)/$@ -conf=./utils/default.conf $(SRC)/cobol/$^

%.cob:
	
clean:
	powershell Remove-Item -Path $(BINS)
	powershell Remove-Item -Path $(DLLS)