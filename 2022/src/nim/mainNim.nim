# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import argparse
import std/parseutils
import dayOne, dayTwo, dayThree, dayFour, dayFive, daySix, daySeven

var parser = newParser:
  option("-d", "--day", choices = @["1", "2", "3", "4", "5", "6", "7"], required = true)
  option("-p", "--part", choices = @["1", "2"], required = true)

when isMainModule:
    var opts = parser.parse()
    var day: int
    discard opts.day.parseInt(day, 0)
    var part: char
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
    else:
        echo "Incorrect day selected."
