# Copyright (C) 2025 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import os

proc computeJoltageFromTwoBanks(batteries: seq[int64], sum_joltage: var int64) =
    var joltage: array[2, int64]
    var indeces: array[2, int64]

    for i in indeces[0]..<(batteries.len()-1):
        if batteries[i] > joltage[0]:
            joltage[0] = batteries[i]
            indeces[1] = i+1

    for i in indeces[1]..<batteries.len():
        if batteries[i] > joltage[1]:
            joltage[1] = batteries[i]

    sum_joltage += (joltage[0] * 10 + joltage[1])

proc computeJoltageFromTwelveBanks(batteries: seq[int64], sum_joltage: var int64) =
    const nBatteries = 12
    var joltage: array[nBatteries, int64]
    var indeces: array[nBatteries, int64]
    var val_joltage: int64 = 0
    
    for i in 0..<nBatteries:
        for j in indeces[i]..<(batteries.len()-nBatteries+1+i):
            if batteries[j] > joltage[i]:
                joltage[i] = batteries[j]
                if i != nBatteries-1: indeces[i+1] = j+1

        val_joltage = val_joltage * 10 + joltage[i]
    
    sum_joltage += val_joltage

proc parseBatteries(line: string, batteries: var seq[int64]): bool =
    for l in line:
        batteries.add(int64(l) - int64('0'))
    return true

proc readFileAndCompute(filename: string, option = '1') =
    var sum_joltage: int64 = 0
    var batteries: seq[int64] = newSeq[int64]()

    let compute = case option:
    of '1':
        computeJoltageFromTwoBanks
    of '2':
        computeJoltageFromTwelveBanks
    else:
        echo "Invalid part"
        return

    for line in filename.lines:
        batteries = @[]
        if not parseBatteries(line, batteries):
            echo "Error parsing ", line
            return
        compute(batteries, sum_joltage)

    echo "TOTAL JOLTAGE: ", sum_joltage

proc main*(part: char) =
    const file = "2025/data/input_day_three"
    let filename = getCurrentDir() / file
    readFileAndCompute(filename, part)

when isMainModule:
    main('1')
