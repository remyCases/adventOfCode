import os, parseutils, std/algorithm

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

proc readFileAndComputeCalories(filename: string) =
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
    echo calorieTotal

when isMainModule:
    const file = "2022/data/input_day_one.txt"
    let filename = getCurrentDir() / file
    readFileAndComputeCalories(filename)