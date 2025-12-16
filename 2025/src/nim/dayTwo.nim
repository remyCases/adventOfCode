# Copyright (C) 2025 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import os, parseutils, sets

type
    Range = object
        s: int64
        e: int64

proc ilog10(e: int64): int64 =
    var ilog: int64 = 0
    var ee: int64 = e
    while ee >= 10:
        ilog += 1
        ee = ee div 10

    return ilog

proc ipow10(e: int64): int64 =
    var ipow: int64 = 1
    var ee: int64 = e
    while ee > 0:
        ipow *= 10
        ee -= 1

    return ipow

proc parseRanges(line: string, ranges: var seq[Range]): bool =
    var i: int = 0
    var s: int64
    var e: int64
    var sep: char

    while i < line.len:
        i.inc parseBiggestInt(line, s, i)
        i.inc parseChar(line, sep, i)
        if sep != '-': return false

        i.inc parseBiggestInt(line, e, i)
        i.inc
        
        ranges.add(Range(s: s, e: e))

    return true

proc find_repeated_twice_ids(s: int64, e: int64): int64 = 
    let n = ilog10(s) + 1
    var ids: HashSet[int64] = initHashSet[int64]()
    var sum: int64 = 0

    if n mod 2 != 0: return 0

    let u = ipow10(n div 2) + 1
    let us: int64 = if (s mod u) == 0: s div u else: (s div u) + 1
    let ue = e div u
    for i in us..ue:
        if not ids.containsOrIncl(i * u):
            sum += i * u

    return sum

proc find_repeated_ids(s: int64, e: int64): int64 = 
    let n = ilog10(s) + 1
    var ids: HashSet[int64] = initHashSet[int64]()
    var sum: int64 = 0

    for d in 2..n:
        if n mod d != 0: continue

        let size_repetition: int64 = n div d
        var u: int64 = 0

        for i in 0..<d:
            u += ipow10(size_repetition * i)

        let us: int64 = if (s mod u) == 0: s div u else: (s div u) + 1
        let ue = e div u
        for i in us..ue:
            if not ids.containsOrIncl(i * u):
                sum += i * u

    return sum

proc readFileAndCompute(filename: string, option = '1') =
    var invalid_ids: int64 = 0
    var ranges: seq[Range] = newSeq[Range]()

    for line in filename.lines:
        if not parseRanges(line, ranges):
            echo "Error parsing ", line
            return
    
    let find = case option:
    of '1':
        find_repeated_twice_ids
    of '2':
        find_repeated_ids
    else:
        echo "Invalid part"
        return

    for r in ranges:
        var s: int64 = r.s
        var ns: int64 = ilog10(s)
        let ne: int64 = ilog10(r.e)
        while ns < ne:
            invalid_ids += find(s, ipow10(ns+1)-1)
            ns += 1
            s = ipow10(ns)

        invalid_ids += find(s, r.e)

    echo "INVALID IDS: ", invalid_ids

proc main*(part: char) =
    const file = "2025/data/input_day_two"
    let filename = getCurrentDir() / file
    readFileAndCompute(filename, part)

when isMainModule:
    main('1')
