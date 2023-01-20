import os, std/[tables, parseutils], math
import ../../utils/argparser
import ../../utils/queue

const upperLetter = { 'A' .. 'Z' }

proc applyCommandOneByOne[T](roots: var seq[Queue[T]], count, src, des: int) =
    var c = count
    while c > 0:
        push(roots[des], popNoFree(roots[src]))
        c.dec

proc applyCommandByChunk[T](roots: var seq[Queue[T]], count, src, des: int) =
    reunite(separateHead(roots[src], count), roots[des])

proc computeTopCrate[T](roots: var seq[Queue[T]]): string =
    for root in roots:
        result &= $headValue(root)

proc parseCargo(line: string, roots: var seq[Queue[char]]) =
    if line.len == 0: return
    for n, l in line.pairs:
        if l in upperLetter:
            let index = euclDiv(n, 4)
            push(roots[index], l)

proc parseHeader(filename: string, roots: var seq[Queue[char]], endOfHeader: int) =
    # iterate backward
    let cargo = filename.readLines(endOfHeader)
    let cargoLen = cargo.len
    roots.setLen(euclDiv(cargo[0].len, 4) + 1)
    for i in 0..<cargoLen:
        parseCargo(cargo[cargoLen - 1 - i], roots)

proc parseCommand(line: string, count, src, des: var int) =
    if line.len == 0: return
    var i = 0
    var str = ""
    i.inc parseUntil(line, str, {' '}, i)
    i.inc
    i.inc parseInt(line, count, i)
    i.inc
    i.inc parseUntil(line, str, {' '}, i)
    i.inc
    i.inc parseInt(line, src, i)
    i.inc
    i.inc parseUntil(line, str, {' '}, i)
    i.inc
    i.inc parseInt(line, des, i)

proc parseFileFromCargoAndCommand(filename: string): int =
    result = 0
    for line in filename.lines:
        if line == "":
            break
        result.inc

proc readFileAndComputeTopCrate(filename: string, roots: var seq[Queue[char]], option = '1') =
    var count, src, des: int
    let indexSeperate = parseFileFromCargoAndCommand(filename)
    parseHeader(filename, roots, indexSeperate)
    echo $roots
    var n = 0
    
    let applyCommand = case option:
    of '1':
        applyCommandOneByOne[char]
    of '2':
        applyCommandByChunk[char]
    else:
        return
    
    for line in filename.lines:
        n.inc
        if n < indexSeperate: continue # pass first lines, it's the header
        
        parseCommand(line, count, src, des)

        if src == 0 or des == 0: continue # src or des can't be 0, it should be a false line
        applyCommand(roots, count, src-1, des-1)
    echo "Top crate: ", computeTopCrate(roots)

when isMainModule:
    const file = "2022/data/input_day_five.txt"
    let filename = getCurrentDir() / file

    const partToken: CmdOption = CmdOption(long: "part", short: "p", required: true, choice: @["1", "2"])
    var t = {partToken: ""}.toTable
    handleTokens(t)
    let part = t[partToken]

    var roots: seq[Queue[char]]
    readFileAndComputeTopCrate(filename, roots, char(part[0]))