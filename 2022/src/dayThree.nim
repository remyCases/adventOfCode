import os, std/[sugar, tables, sets], math
import ../../../Nim/ArgParser/argparser

const lowerLetter: HashSet[char] = collect(initHashSet()):
    for i in {'a' .. 'z'}: {i}

const upperLetter: HashSet[char] = collect(initHashSet()):
    for i in {'A' .. 'Z'}: {i}

const firstLowerLetterHex = 0x61
const firstUpperLetterHex = 0x41

const COUNTER_MAX = 3
var counter = 0

proc computePriority(intersectContent: HashSet[char]): int =
    var priority = 0
    for i in intersectContent * lowerLetter:
        priority += int(i) - firstLowerLetterHex + 1
    for i in intersectContent * upperLetter:
        priority += int(i) - firstUpperLetterHex + 27
    return priority

proc computePriorityEveryThird(intersectContent: HashSet[char]): int =
    if counter < COUNTER_MAX: return 0
    var priority = 0
    for i in intersectContent * lowerLetter:
        priority += int(i) - firstLowerLetterHex + 1
    for i in intersectContent * upperLetter:
        priority += int(i) - firstUpperLetterHex + 27
    return priority

proc parseInTwoContents(line: string, intersect: var HashSet[char]) =
    if line.len == 0: return
    let halfLineLen: int = euclDiv[int](line.len, 2)

    let firstRucksacksContent = line[0..<halfLineLen].toHashSet()
    let secondRucksacksContent = line[halfLineLen..^1].toHashSet()
    intersect = firstRucksacksContent * secondRucksacksContent

proc parseInOneContents(line: string, intersect: var HashSet[char]) =
    if line.len == 0: return
    if counter >= COUNTER_MAX:
        counter = 0
        intersect = line.toHashSet()
    else:
        intersect = intersect * line.toHashSet()
    counter.inc

proc readFileAndComputePriority(filename: string, option = '1') =
    var priority = 0
    var intersect = collect(initHashSet()):
        for i in {'a' .. 'z', 'A' .. 'Z'}: {i}

    let procCompute = case option:
    of '1':
        computePriority
    of '2':
        computePriorityEveryThird
    else:
        return

    let parse = case option:
    of '1':
        parseInTwoContents
    of '2':
        parseInOneContents
    else:
        return

    for line in filename.lines:
        parse(line, intersect)
        priority += procCompute(intersect)
    echo priority

when isMainModule:
    const file = "2022/data/input_day_three.txt"
    let filename = getCurrentDir() / file

    const partToken: CmdOption = CmdOption(long: "part", short: "p", required: true, choice: @["1", "2"])
    var t = {partToken: ""}.toTable
    handleTokens(t)
    let part = t[partToken]

    readFileAndComputePriority(filename, char(part[0]))