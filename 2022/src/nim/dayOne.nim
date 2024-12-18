# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import os, parseutils, std/algorithm

const bufferLen: int = 3
var bufferInt: array[bufferLen, int] = [0, 0, 0]

proc addNewInt(newValue: int) =
    if newValue > bufferInt[0]:
        bufferInt[0] = newValue
        bufferInt.sort()

proc parse(line: string, calorieCount: var int) =
    if line.len == 0: return
    var i = 0
    i.inc parseInt(line, caloriecount, i)

proc readFileAndComputeCalories(filename: string, option = '1') =
    var calorieCount = 0
    var calorieCumul = 0
    for line in filename.lines:
        if line.len == 0:
            addNewInt(calorieCumul)
            calorieCumul = 0
        else:
            parse(line, calorieCount)
            calorieCumul += calorieCount

    var calorieTotal = 0
    case option:
    of '1':
        calorieTotal = bufferInt[bufferLen - 1]
        echo "MAX CALORIES: ", calorieTotal
    of '2':
        for i in bufferInt:
            calorieTotal += i
        echo "MAX CALORIES: ", calorieTotal
    else:
        return

proc main*(part: char) =
    const file = "2022/data/input_day_one"
    let filename = getCurrentDir() / file
    readFileAndComputeCalories(filename, part)

when isMainModule:
    main('1')
