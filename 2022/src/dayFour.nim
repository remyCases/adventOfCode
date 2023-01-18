import os, std/[tables, parseutils]
import ../../../Nim/ArgParser/argparser

proc computeTotalOverlap(startFirstRange, endFirstRange, startSecondRange, endSecondRange: int): int =
    if (startFirstRange <= startSecondRange) and (endFirstRange >= endSecondRange): # A B B A
        return 1
    elif (startFirstRange >= startSecondRange) and (endFirstRange <= endSecondRange): # B A A B
        return 1
    return 0

proc computePartialOverlap(startFirstRange, endFirstRange, startSecondRange, endSecondRange: int): int =
    if (startFirstRange < startSecondRange) and (endFirstRange < startSecondRange): # AA - BB
        return 0
    elif (startSecondRange < startFirstRange) and (endSecondRange < startFirstRange): # BB - AA
        return 0
    return 1

proc parse(line: string, startFirstRange, endFirstRange, startSecondRange, endSecondRange: var int) =
    if line.len == 0: return
    var i = 0
    i.inc parseInt(line, startFirstRange, i)
    i.inc
    i.inc parseInt(line, endFirstRange, i)
    i.inc
    i.inc parseInt(line, startSecondRange, i)
    i.inc
    i.inc parseInt(line, endSecondRange, i)

proc readFileAndComputePriority(filename: string, option = '1') =
    var startFirstRange, endFirstRange, startSecondRange, endSecondRange = 0
    var overlapCount = 0

    let procCompute = case option:
    of '1':
        computeTotalOverlap
    of '2':
        computePartialOverlap
    else:
        return

    for line in filename.lines:
        parse(line, startFirstRange, endFirstRange, startSecondRange, endSecondRange)
        let overlap = procCompute(startFirstRange, endFirstRange, startSecondRange, endSecondRange)
        overlapCount += overlap
    echo "overlapCount: ", overlapCount

when isMainModule:
    const file = "2022/data/input_day_four.txt"
    let filename = getCurrentDir() / file

    const partToken: CmdOption = CmdOption(long: "part", short: "p", required: true, choice: @["1", "2"])
    var t = {partToken: ""}.toTable
    handleTokens(t)
    let part = t[partToken]

    readFileAndComputePriority(filename, char(part[0]))