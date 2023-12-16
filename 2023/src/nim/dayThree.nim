# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import os, parseutils

const digits = { '1' .. '9' }

type
    Part = object
        value: int
        x: int
        y: int
        size: int

    Seq = object
        x: int
        y: int
        value: char

proc parseAdjacent(line: string, parts: var seq[Part], seqs: var seq[Seq], token: var int, nline: var int) =
    var i = 0
    var size = 0
    while(i < line.len):
        i.inc skipWhile(line, {'.'}, i)
        if i >= line.len: break
        if line[i] in digits:
            size = parseInt(line, token, i)
            parts.add(Part(size: size, x: i, y: nline, value: token))
            i.inc(size)
        else:
            seqs.add(Seq(x: i, y: nline, value: line[i]))
            i.inc

proc readFileComputeAdjacent(filename: string, option = '1') =
    var possibleParts: seq[Part]
    var parts: seq[Part]
    var seqs: seq[Seq]
    var token = 0
    var result = 0
    var nline = 0

    for line in filename.lines:
        parseAdjacent(line, possibleParts, seqs, token, nline)
        nline.inc
        
    for p in possibleParts:
        for s in seqs:
            if s.y == p.y-1 or s.y == p.y or s.y == p.y+1:
                if s.x >= p.x - 1 and s.x <= p.x + p.size:
                    parts.add(p)
                    break
            if s.y == p.y+2:
                break
    
    if option == '1':
        for p in parts:
            result += p.value
            echo result, " ", p.value
    elif option == '2':
        var gear1 = 0
        var gear2 = 0
        for s in seqs:
            if s.value == '*':
                for p in parts:
                    if s.y == p.y-1 or s.y == p.y or s.y == p.y+1:
                        if s.x >= p.x - 1 and s.x <= p.x + p.size:
                            if gear1 == 0:
                                gear1 = p.value
                            else:
                                gear2 = p.value
                                break
                result += gear1*gear2
                gear1 = 0
                gear2 = 0
    else:
        return
    

    echo "Sum parts: ", result

proc main*(part: char) =
    const file = "2023/data/input_day_three"
    let filename = getCurrentDir() / file
    readFileComputeAdjacent(filename, part)

when isMainModule:
    main('1')