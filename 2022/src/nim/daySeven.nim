import os, std/strutils, std/parseutils
import ../../../utils/triplelinkedtree

const MAX_VALUE_SIZE = 100000

type
    ParseType = enum
        file, folder, ls_command, cd_command

proc parseFile(line: string, parseType: var ParseType, name: var string, size: var int) =
    if '$' in line:
        let index_cd: int = line.find("cd")
        let index_ls: int = line.find("ls")
        if index_cd != -1:
            parseType = cd_command
            name = line[index_cd + 3..^1]
        elif index_ls != -1:
            parseType = ls_command
    else:
        let index_dir: int = line.find("dir")
        if index_dir != -1:
            parseType = folder
            name = line[index_dir + 4..^1]
        else:
            parseType = file
            var i = 0
            i.inc parseInt(line, size, i)
            name = line[i + 1..^1]

proc readFileAndComputeFolderSize(filename: string, option = '1') =
    var 
        parseType: ParseType
        name: string
        size: int
        head = newNode[int]("home", 0)
        it: TripleLinkedTreeNode[int]
        was_ls_before = false
        array_data_tree = newSeq[int]()
        array_name_tree = newSeq[string]()
        sum_size = 0

    it = head
    for line in filename.lines:
        parseFile(line, parseType, name, size)
        case parseType:
        of folder:
            if was_ls_before: it.addChild(name, 0)
        of file:
            if was_ls_before: it.data += size
        of ls_command:
            was_ls_before = true
        of cd_command:
            was_ls_before = false
            if name == "..":
                it = it.parent
            else:
                discard it.toDirectly(it, name)

    sumPostorder(head)
    trasversePreorderData(head, array_data_tree)
    trasversePreorderName(head, array_name_tree)
    echo array_name_tree
    for i in array_data_tree:
        if i < MAX_VALUE_SIZE: sum_size += i
    echo sum_size

proc main*(part: char) =
    const file = "2022/data/input_day_seven.txt"
    let filename = getCurrentDir() / file
    readFileAndComputeFolderSize(filename, part)

when isMainModule:
    main('1')