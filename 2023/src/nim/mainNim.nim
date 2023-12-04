# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import argparse
import std/parseutils
import dayOne
import dayTwo
import dayThree

when isMainModule:
    var parser = newParser:
        option("-d", "--day", choices = @["1", "2", "3"], required = true)
        option("-p", "--part", choices = @["1", "2"], required = true)

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
    else:
        echo "Incorrect day selected."