# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

BIN = build/2022/bin/
BINS = $(wildcard $(BIN)/*.exe)
DLLS = $(wildcard build/2022/lib/*.dll)

NIMFLAGS = -o=$(BIN) -d=release --nimcache=build/2022/nimcache --hints=on

build_nim:
	nim c $(NIMFLAGS) ./2022/src/nim/mainNim.nim

clean:
	powershell Remove-Item -Path $(BINS)
	powershell Remove-Item -Path $(DLLS)
