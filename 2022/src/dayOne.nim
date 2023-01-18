import os, parseutils, std/[algorithm, tables]
import ../../utils/argparser

const bufferLen: int = 3
var bufferInt: array[bufferLen, int] = [0, 0, 0]

proc addNewInt(newValue: int) =
    if newValue > bufferInt[0]:
        bufferInt[0] = newValue
        bufferInt.sort()

proc parse(line: string, calorieCount: var int) =
    if line.len == 0: return
    var i = 0
    i.inc parseInt(line, caloriecount, i)

proc readFileAndComputeCalories(filename: string, option = '1') =
    var calorieCount = 0
    var calorieCumul = 0
    for line in filename.lines:
        if line.len == 0:
            addNewInt(calorieCumul)
            calorieCumul = 0
        else:
            parse(line, calorieCount)
            calorieCumul += calorieCount
    echo bufferInt
    var calorieTotal = 0
    for i in bufferInt:
        calorieTotal += i
    echo "calorieTotal: ", calorieTotal

when isMainModule:
    const file = "2022/data/input_day_one.txt"
    let filename = getCurrentDir() / file

    const partToken: CmdOption = CmdOption(long: "part", short: "p", required: true, choice: @["1", "2"])
    var t = {partToken: ""}.toTable
    handleTokens(t)
    let part = t[partToken]

    readFileAndComputeCalories(filename, char(part[0]))