# Copyright (C) 2025 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import ast
from dataclasses import dataclass
from pathlib import Path

@dataclass
class Rotation:
    increment: int = 0
    orientation: int = 0
    cycles: int = 0

MAX_DIAL = 100
LEFT = 1
RIGHT = -1

def parseRotation(line: str) -> Rotation:
    r = Rotation()
    if line[0] == 'L':
        r.orientation = LEFT
    elif line[0] == 'R':
        r.orientation = RIGHT
    else:
        raise ValueError(f"Invalid line to parse: {line}.")

    val = ast.literal_eval(line[1:])

    r.increment = val % MAX_DIAL
    r.cycles = val // MAX_DIAL
    return r

def readFileAndCompute(filename: Path, option: int = 1) -> None:
    dial: int = 50
    dial_at_zero: int = 0

    with open(filename, 'r') as file:
        for line in file:
            rotation = parseRotation(line)

            if option == 1:
                if rotation.increment == 0:
                    continue

                if rotation.orientation == LEFT:
                    dial = (dial + MAX_DIAL - rotation.increment) % MAX_DIAL
                elif rotation.orientation == RIGHT:
                    dial = (dial + rotation.increment) % MAX_DIAL
                if dial == 0:
                    dial_at_zero += 1

            elif option == 2:
                dial_at_zero += rotation.cycles
                if rotation.increment == 0:
                    continue
    
                if rotation.orientation == LEFT:
                    if dial <= rotation.increment and dial != 0:
                        dial_at_zero += 1
                    dial = (dial + MAX_DIAL - rotation.increment) % MAX_DIAL
                elif rotation.orientation == RIGHT:
                    if dial + rotation.increment >= MAX_DIAL:
                        dial_at_zero += 1
                    dial = (dial + rotation.increment) % MAX_DIAL
            else:
                print("Invalid part")
                return

    print(f"DIAL AT ZERO: {dial_at_zero}")

def main(part: int) -> None:
    file = Path("2025/data/input_day_one")
    filename = Path.cwd() / file
    readFileAndCompute(filename, part)
