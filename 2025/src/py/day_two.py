# Copyright (C) 2025 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import ast
import math
from pathlib import Path
from typing import List
from dataclasses import dataclass

@dataclass
class Range:
    s: int
    e: int

def parseRanges(line: str) -> List[Range]:
    ranges: List[Range] = []
    
    pairs_ranges = line.split(',')
    for pair in pairs_ranges:
        str_e, str_s = pair.split('-')
        ranges.append(
            Range(ast.literal_eval(str_e), ast.literal_eval(str_s))
        )

    return ranges

def find_repeated_twice_ids(s: int, e: int) -> int:
    n = int(math.log10(s)) + 1
    ids: set[int] = set()
    sum_range: int = 0

    if n % 2 != 0:
        return 0

    u = 10 ** (n // 2) + 1
    us: int = s // u if (s % u) == 0 else (s // u) + 1
    ue: int = e // u + 1
    for i in range(us, ue):
        new_entry = i * u
        if new_entry not in ids:
            ids.add(new_entry)
            sum_range += new_entry

    return sum_range

def find_repeated_ids(s: int, e: int) -> int:
    n = int(math.log10(s)) + 1
    ids: set[int] = set()
    sum_range: int = 0

    for d in range(2, n + 1):
        if n % d != 0:
            continue

        size_repetition: int = n // d
        u: int = 0

        for i in range(0, d):
            u += 10 ** (size_repetition * i)

        us: int = s // u if (s % u) == 0 else (s // u) + 1
        ue: int = e // u + 1
        for i in range(us, ue):
            new_entry = i * u
            if new_entry not in ids:
                ids.add(new_entry)
                sum_range += new_entry

    return sum_range

def readFileAndCompute(filename: Path, option: int = 1) -> None:
    invalid_ids: int = 0
    ranges: List[Range] = []

    with open(filename, 'r') as file:
        for line in file:
            ranges = parseRanges(line)
    
    find = None
    if option == 1:
        find = find_repeated_twice_ids
    elif option == 2:
        find = find_repeated_ids
    else:
        print("Invalid part")
        return

    for r in ranges:
        s: int = r.s
        ns: int = int(math.log10(s)) + 1
        ne: int = int(math.log10(r.e)) + 1
        while ns < ne:
            invalid_ids += find(s, (10 ** ns)-1)
            s = 10 ** ns
            ns += 1

        invalid_ids += find(s, r.e)

    print(f"INVALID IDS: {invalid_ids}")

def main(part: int) -> None:
    file = Path("2025/data/input_day_two")
    filename = Path.cwd() / file
    readFileAndCompute(filename, part)
