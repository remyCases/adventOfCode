# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import os
import std/parseopt, std/tables
import system

type
    CmdOption* = object
        long*: string
        short*: string
        required*: bool
        choice*: seq[string]

proc contains[CmdOption, string](t: Table[CmdOption, string], key: string): bool =
    for k, _ in t:
        if k.long == key or k.short == key:
            return true
    return false

proc `[]=`[CmdOption, string](t: var Table[CmdOption, string], key: string, val: sink string) =
    for k, v in t:
        if k.long == key or k.short == key:
            if k.choice.len == 0 or val in k.choice:
                t[k] = val
            else:
                raise newException(OSError, "Incorrect options")
            return

proc handleSingleToken(table: var Table[CmdOption, string], 
    kind: CmdLineKind, key: string, val: string) =
  case kind
  of cmdEnd:
    doAssert(false)  # Doesn't happen with getopt()

  of cmdShortOption, cmdLongOption:
    if key in table:
        `[]=`(table, key, val)

  of cmdArgument:
    echo "Argument: ", key

proc handleTokens*(table: var Table[CmdOption, string]) =
    var opt_parser = initOptParser(quoteShellCommand(commandLineParams()))
    for kind, key, val in opt_parser.getopt():
        handleSingleToken(table, kind, key, val)