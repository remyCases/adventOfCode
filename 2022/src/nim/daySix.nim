# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import os
import ../../../utils/linkedqueue

proc readFileAndComputeMarker(filename: string, option = '1') =
    var queue: LinkedQueue[char] = newLinkedQueue[char]()
    var queueLen = 0
    let values = filename.open(fmRead).readAll()

    let maxQueue = case option:
    of '1':
        4
    of '2':
        14
    else:
        return

    for n, v in values.pairs():
        while v in queue:
            queue.pop()
            queueLen -= 1

        queue.push(v)
        queueLen += 1
        if queueLen == maxQueue:
            echo n + 1
            break

proc main*(part: char) =
    const file = "2022/data/input_day_six.txt"
    let filename = getCurrentDir() / file
    readFileAndComputeMarker(filename, part)

when isMainModule:
    main('1')
