# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import os, algorithm

const digits = { 1 .. 9 }
const zero = int('0')
const validAnswer = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

proc validAnswerToInt(va: string, digit: var int) =
    if va == "1" or va == "one":
        digit = 1
    elif va == "2" or va == "two":
        digit = 2
    elif va == "3" or va == "three":
        digit = 3
    elif va == "4" or va == "four":
        digit = 4
    elif va == "5" or va == "five":
        digit = 5
    elif va == "6" or va == "six":
        digit = 6
    elif va == "7" or va == "seven":
        digit = 7
    elif va == "8" or va == "eight":
        digit = 8
    elif va == "9" or va == "nine":
        digit = 9

proc parseNaive(line: string, firstDigit: var int, secondDigit: var int) =
    if line.len == 0: return
    for c in line:
        if int(c)-zero in digits:
            firstDigit = int(c)-zero
            break
    
    for c in line.reversed():
        if int(c)-zero in digits:
            secondDigit = int(c)-zero
            break

proc parseWithString(line: string, firstDigit: var int, secondDigit: var int) =
    var low = line.low
    var high = low
    let len = line.len
    while(low < len):
        if line[low .. high] in validAnswer:
            validAnswerToInt(line[low .. high], firstDigit)
            break
        high = high + 1
        if high == len or high - low > 5: # there is no element bigger than a six letters string
            low = low + 1
            high = low
    
    low = line.high
    high = low
    while(high >= 0):
        if line[low .. high] in validAnswer:
            validAnswerToInt(line[low .. high], secondDigit)
            break
        low = low - 1
        if low == -1 or high - low > 5:
            high = high - 1
            low = high


proc readFileAndCalibration(filename: string, option = '1') =
    var firstDigit: int = 0
    var secondDigit: int = 0
    var calibrationValue: int = 0

    let parse = case option:
    of '1':
        parseNaive
    of '2':
        parseWithString
    else:
        return

    for line in filename.lines:
        parse(line, firstDigit, secondDigit)
        calibrationValue += firstDigit*10 + secondDigit

    echo "CALIBRATION VALUE: ", calibrationValue

proc main*(part: char) =
    const file = "2023/data/input_day_one"
    let filename = getCurrentDir() / file
    readFileAndCalibration(filename, part)

when isMainModule:
    main('1')
