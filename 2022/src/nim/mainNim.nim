# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import argparse
import std/parseutils
import std/strformat
import dayOne, dayTwo, dayThree, dayFour, dayFive, daySix, daySeven, dayEight

when isMainModule:
    var parser = newParser:
        option("-d", "--day", choices = @["1", "2", "3", "4", "5", "6", "7", "8"], required = true)
        option("-p", "--part", choices = @["1", "2"], required = true)

    var day: int
    var part: char

    try:
        var opts = parser.parse()
        discard opts.day.parseInt(day, 0)
        discard opts.part.parseChar(part, 0)

        case day:
        of 1:
            dayOne.main(part)
        of 2:
            dayTwo.main(part)
        of 3:
            dayThree.main(part)
        of 4:
            dayFour.main(part)
        of 5:
            dayFive.main(part)
        of 6:
            daySix.main(part)
        of 7:
            daySeven.main(part)
        of 8:
            dayEight.main(part)
        else:
            discard
    except UsageError:
        echo &"Incorrect combination of day and part. Given day and part does not exist (yet)."

