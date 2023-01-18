import os, std/[sugar, tables, sets], math
import ../../../Nim/ArgParser/argparser

const lowerLetter: HashSet[char] = collect(initHashSet()):
    for i in {'a' .. 'z'}: {i}

const upperLetter: HashSet[char] = collect(initHashSet()):
    for i in {'A' .. 'Z'}: {i}

const firstLowerLetterHex = 0x61
const firstUpperLetterHex = 0x41

proc computePriority(intersectContent: HashSet[char]): int =
    var priority = 0
    for i in intersectContent * lowerLetter:
        priority += int(i) - firstLowerLetterHex + 1
    for i in intersectContent * upperLetter:
        priority += int(i) - firstUpperLetterHex + 27
    return priority

proc parse(line: string, firstRucksacksContent, secondRucksacksContent: var HashSet[char]) =
    if line.len == 0: return
    let halfLineLen: int = euclDiv[int](line.len, 2)

    firstRucksacksContent = line[0..<halfLineLen].toHashSet()
    secondRucksacksContent = line[halfLineLen..^1].toHashSet()


proc readFileAndComputePriority(filename: string, option = '1') =
    var priority = 0
    var firstRucksacksContent = HashSet[char]()
    var secondRucksacksContent = HashSet[char]()

    let procCompute = case option:
    of '1':
        computePriority
    of '2':
        computePriority
    else:
        return

    for line in filename.lines:
        parse(line, firstRucksacksContent, secondRucksacksContent)
        let intersect =  firstRucksacksContent * secondRucksacksContent

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