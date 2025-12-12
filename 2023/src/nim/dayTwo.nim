# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import os, parseutils

const MAX_RED = 12
const MAX_GREEN = 13
const MAX_BLUE = 14

const digits = {'0' .. '9'}

proc incAndRetrun(i: var int, j: int): int =
    i += j
    return j

proc parse(line: string, gameId: var int, tmp: var int, token: var string, maxRed: var int, maxBlue: var int, maxGreen: var int) =
    var i = skipUntil(line, digits, 0)
    maxRed = 0
    maxGreen = 0
    maxBlue = 0
    i.inc parseInt(line, gameId, i)
    i += 2
    
    while(incAndRetrun(i, parseInt(line, tmp, i)) != 0):
        i.inc parseUntil(line, token, {'r', 'b', 'g'}, i)
        i.inc parseUntil(line, token, {';', ','}, i)
        if token == "red":
            if tmp > maxRed:
                maxRed = tmp
        elif token == "blue":
            if tmp > maxBlue:
                maxBlue = tmp
        elif token == "green":
            if tmp > maxGreen:
                maxGreen = tmp
        else:
            echo "Not recognised token: ", token
        i.inc parseUntil(line, token, {'0'.. '9'}, i)

proc computeSumId(maxRed: int, maxBlue: int, maxGreen: int, gameId: int, sumId: var int) =
    if maxRed <= MAX_RED and maxBlue <= MAX_BLUE and maxGreen <= MAX_GREEN:
            sumId += gameId

proc computePower(maxRed: int, maxBlue: int, maxGreen: int, gameId: int, power: var int) =
    power += maxRed * maxBlue * maxGreen

proc readComputeResult(filename: string, option = '1') =
    var tmp = 0
    var token = ""
    var result = 0
    var gameId = 0
    var maxRed = 0
    var maxGreen = 0
    var maxBlue = 0

    var compute = case option:
    of '1':
        computeSumId
    of '2':
        computePower
    else:
        return

    for line in filename.lines:
        parse(line, gameId, tmp, token, maxRed, maxBlue, maxGreen)
        compute(maxRed, maxBlue, maxGreen, gameId, result)
        
    echo "POWER OF GAMES: ", result

proc main*(part: char) =
    const file = "2023/data/input_day_two"
    let filename = getCurrentDir() / file
    readComputeResult(filename, part)

when isMainModule:
    main('1')
