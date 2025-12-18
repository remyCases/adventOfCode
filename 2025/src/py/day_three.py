# Copyright (C) 2025 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import ast
from pathlib import Path
from typing import List

NUMBER_BATTERIES = 12

def computeJoltageFromTwoBanks(batteries: List[int]) -> int:
    joltage: List[int] = [0, 0]
    indeces: List[int] = [0, 0]

    for i in range(indeces[0], (len(batteries)-1)):
        if batteries[i] > joltage[0]:
            joltage[0] = batteries[i]
            indeces[1] = i+1

    for i in range(indeces[1], len(batteries)):
        if batteries[i] > joltage[1]:
            joltage[1] = batteries[i]

    return joltage[0] * 10 + joltage[1]

def computeJoltageFromTwelveBanks(batteries: List[int]) -> int:
    
    joltage: List[int] = [0]*NUMBER_BATTERIES
    indeces: List[int] = [0]*NUMBER_BATTERIES
    val_joltage: int = 0
    
    for i in range(0, NUMBER_BATTERIES):
        for j in range(indeces[i], (len(batteries)-NUMBER_BATTERIES+1+i)):
            if batteries[j] > joltage[i]:
                joltage[i] = batteries[j]
                if i != NUMBER_BATTERIES-1:
                    indeces[i+1] = j+1

        val_joltage = val_joltage * 10 + joltage[i]
    
    return val_joltage

def parseBatteries(line: str) -> List[int]:
    batteries: List[int] = []
    for ll in line:
        if ll == '\n':
            break
        batteries.append(ast.literal_eval(ll))
    return batteries

def readFileAndCompute(filename: Path, option: int = 1) -> None:
    sum_joltage: int = 0

    with open(filename, 'r') as file:
        for line in file:
            batteries = parseBatteries(line)
            if option == 1:
                sum_joltage += computeJoltageFromTwoBanks(batteries)
            elif option == 2:
                sum_joltage += computeJoltageFromTwelveBanks(batteries)
            else:
                print("Invalid part")
                return

    print(f"TOTAL JOLTAGE: {sum_joltage}")

def main(part: int) -> None:
    file = Path("2025/data/input_day_three")
    filename = Path.cwd() / file
    readFileAndCompute(filename, part)
