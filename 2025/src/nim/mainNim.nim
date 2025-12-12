# Copyright (C) 2025 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import argparse
import std/strformat
import std/parseutils
import dayOne
import dayTwo

when isMainModule:
    var parser = newParser:
        option("-d", "--day", choices = @["1", "2"], required = true)
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
        else:
            discard
    except UsageError:
        echo &"Incorrect combination of day and part. Given day and part does not exist (yet)."
