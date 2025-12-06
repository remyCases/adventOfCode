# Copyright (C) 2025 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import os, parseutils

const MAX_DIAL = 100

type
    Orientations = enum
        Left, Right

proc parseRotation(line: string, increment: var int, orientation: var Orientations, cycles: var int): bool =
    var o: char
    var i: int = parseChar(line, o, 0)

    case o:
    of 'L':
        orientation = Orientations.Left
    of 'R':
        orientation = Orientations.Right
    else:
        return false

    var val: int = 0
    i.inc parseInt(line, val, i)

    increment = val mod MAX_DIAL
    cycles = val div MAX_DIAL
    return true


proc readFileAndCompute(filename: string, option = '1') =
    var increment: int = 0
    var orientation: Orientations
    var cycles: int = 0
    var dial: int = 50
    var dial_at_zero: int = 0

    for line in filename.lines:
        if not parseRotation(line, increment, orientation, cycles):
            echo "Error parsing ", line
            return

        case option:
        of '1':
            if increment == 0: continue

            case orientation:
            of Orientations.Left:
                dial = (dial + MAX_DIAL - increment) mod MAX_DIAL
            of Orientations.Right:
                dial = (dial + increment) mod MAX_DIAL
            if dial == 0: dial_at_zero.inc
        of '2':
            dial_at_zero += cycles
            if increment == 0: continue
            case orientation:
            of Orientations.Left:
                if dial <= increment and dial != 0: dial_at_zero.inc
                dial = (dial + MAX_DIAL - increment) mod MAX_DIAL
            of Orientations.Right:
                if dial + increment >= MAX_DIAL: dial_at_zero.inc
                dial = (dial + increment) mod MAX_DIAL
        else:
            return

    echo "DIAL AT ZERO: ", dial_at_zero

proc main*(part: char) =
    const file = "2025/data/input_day_one"
    let filename = getCurrentDir() / file
    readFileAndCompute(filename, part)

when isMainModule:
    main('1')
