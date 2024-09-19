# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import os, parseutils

# 0..2 opponent choose rock
    # 0 I choose rock
    # 1 I choose paper
    # 2 I choose scissor
# 3..5 opponent choose paper
# 6..8 opponent choose scissor
const resultArray: array[9, int] = [
    3, 6, 0,
    0, 3, 6,
    6, 0, 3
]

# 0..2 opponent choose rock
    # 0 I loose, so I chosed scissor
    # 1 tie, so I chosed rock
    # 2 I win, so I chosed paper
# 3..5 opponent choose paper
# 6..8 opponent choose scissor
const pointFromYourChoiceArray: array[9, int] = [
    3, 1, 2,
    1, 2, 3,
    2, 3, 1
]

const Aint = 0x41
const Xint = 0x58

proc parse(line: string, opponentStrat, yourStrat: var char) =
    if line.len == 0: return
    var i = 0
    i.inc parseChar(line, opponentStrat, i)
    i.inc
    i.inc parseChar(line, yourStrat, i)

proc computePointWhenSecondIsStrat*(opponentStrat, yourStrat: char): int =
    let opponentIndex = int(opponentStrat) - Aint
    let yourIndex = int(yourStrat) - Xint

    let index = 3 * opponentIndex + yourIndex
    result = resultArray[index] + yourIndex + 1

proc computePointWhenSecondIsResult*(opponentStrat, yourResult: char): int =
    let opponentIndex = int(opponentStrat) - Aint
    let yourIndex = int(yourResult) - Xint

    result = yourIndex * 3 + pointFromYourChoiceArray[opponentIndex * 3 + yourIndex]

proc readFileAndComputeScores(filename: string, option = '1') =
    var opponentStrat = '\0'
    var yourStrat = '\0'
    var score = 0

    let procCompute = case option:
    of '1':
        computePointWhenSecondIsStrat
    of '2':
        computePointWhenSecondIsResult
    else:
        return

    for line in filename.lines:
        parse(line, opponentStrat, yourStrat)
        let point = procCompute(opponentStrat, yourStrat)
        score += point
    echo "SCORE: ", score

proc main*(part: char) =
    const file = "2022/data/input_day_two.txt"
    let filename = getCurrentDir() / file
    readFileAndComputeScores(filename, part)

when isMainModule:
    main('1')
