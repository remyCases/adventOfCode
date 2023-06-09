# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import os, parseutils, std/options

type 
    GridLine = object
        count_one_way: int
        count_other_way: int
        last_value: Option[int]

proc readFileAndComputeVisibleTrees(filename: string, option = '1') =
    let n = 5
    var 
        grid_line = GridLine(
            count_one_way: 0,
            count_other_way: 0,
            last_value: none(int),
        )
        parsed_char = 0

    for line in filename.lines:
        grid_line.count_one_way = 0
        grid_line.count_other_way = n
        grid_line.last_value = none(int)

        for ch in line:
            parsed_char = ord(ch) - ord('0')

            if grid_line.last_value.isNone():
                grid_line.last_value = some(parsed_char)
            else:
                if parsed_char > grid_line.last_value.get():
                    grid_line.count_one_way += 1
                    grid_line.count_other_way -= 1
                grid_line.last_value = some(parsed_char)
        echo grid_line


proc main*(part: char) =
    const file = "2022/data/input_day_eight.txt"
    let filename = getCurrentDir() / file
    readFileAndComputeVisibleTrees(filename, part)

when isMainModule:
    main('1')