# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import ../src/nim/daySeven
import ../../utils/triplelinkedtree

block OnlyHead:
    var 
        parseType: ParseType
        name: string
        size: int = 0
        head = newNode[int]("", 0)
        it: TripleLinkedTreeNode[int]
        was_ls_before = false
        array_tree = newSeq[(string, int)]()

    let line = "$ cd m"
    it = head
    processLine(line, it, parseType, name, size, was_ls_before)
    trasversePreorder(it, array_tree)
    doAssert parseType == ParseType.cd_command
    doAssert name == "m"
    doAssert size == 0
    doAssert was_ls_before == false
    doAssert array_tree.len() == 1
    doAssert array_tree == @[("m", 0)]

block OnlyCD:
    var 
        parseType: ParseType
        name: string
        size: int = 0
        head = newNode[int]("home", 0)
        it: TripleLinkedTreeNode[int]
        was_ls_before = false
        array_tree = newSeq[(string, int)]()

    let line = "$ cd m"
    it = head
    processLine(line, it, parseType, name, size, was_ls_before)
    trasversePreorder(head, array_tree)
    doAssert parseType == ParseType.cd_command
    doAssert name == "m"
    doAssert size == 0
    doAssert was_ls_before == false
    doAssert array_tree.len() == 1
    doAssert it == head

block OnlyLS:
    var 
        parseType: ParseType
        name: string = ""
        size: int = 0
        head = newNode[int]("home", 0)
        it: TripleLinkedTreeNode[int]
        was_ls_before = false
        array_tree = newSeq[(string, int)]()

    let line = "$ ls"
    it = head
    processLine(line, it, parseType, name, size, was_ls_before)
    trasversePreorder(head, array_tree)
    doAssert parseType == ParseType.ls_command
    doAssert name == ""
    doAssert size == 0
    doAssert was_ls_before == true
    doAssert array_tree.len() == 1
    doAssert it == head

block OnlyDir:
    var 
        parseType: ParseType
        name: string = ""
        size: int = 0
        head = newNode[int]("home", 0)
        it: TripleLinkedTreeNode[int]
        was_ls_before = false
        array_tree = newSeq[(string, int)]()

    let line = "dir test"
    it = head
    processLine(line, it, parseType, name, size, was_ls_before)
    trasversePreorder(head, array_tree)
    doAssert parseType == ParseType.folder
    doAssert name == "test"
    doAssert size == 0
    doAssert was_ls_before == false
    doAssert array_tree.len() == 1
    doAssert it == head

block OnlyFile:
    var 
        parseType: ParseType
        name: string = ""
        size: int = 0
        head = newNode[int]("home", 0)
        it: TripleLinkedTreeNode[int]
        was_ls_before = false
        array_tree = newSeq[(string, int)]()

    let line = "34 yu.test"
    it = head

    processLine(line, it, parseType, name, size, was_ls_before)
    trasversePreorder(head, array_tree)
    doAssert parseType == ParseType.file
    doAssert name == "yu.test"
    doAssert size == 34
    doAssert was_ls_before == false
    doAssert array_tree.len() == 1
    doAssert it == head

block DirWithLS:
    var 
        parseType: ParseType
        name: string = ""
        size: int = 0
        head = newNode[int]("home", 0)
        it: TripleLinkedTreeNode[int]
        was_ls_before = false
        array_tree = newSeq[(string, int)]()
    it = head

    processLine("$ ls", it, parseType, name, size, was_ls_before)
    doAssert parseType == ParseType.ls_command
    doAssert name == ""
    doAssert size == 0
    doAssert was_ls_before == true

    processLine("dir m1", it, parseType, name, size, was_ls_before)
    doAssert parseType == ParseType.folder
    doAssert name == "m1"
    doAssert size == 0
    doAssert was_ls_before == true

    processLine("dir m2", it, parseType, name, size, was_ls_before)
    doAssert parseType == ParseType.folder
    doAssert name == "m2"
    doAssert size == 0
    doAssert was_ls_before == true
    
    processLine("dir m3", it, parseType, name, size, was_ls_before)
    doAssert parseType == ParseType.folder
    doAssert name == "m3"
    doAssert size == 0
    doAssert was_ls_before == true

    trasversePreorder(head, array_tree)
    doAssert array_tree.len() == 4
    doAssert array_tree == @[("home", 0), ("m1", 0), ("m2", 0), ("m3", 0)]

block DirThenCD:
    var 
        parseType: ParseType
        name: string = ""
        size: int = 0
        head = newNode[int]("home", 0)
        it: TripleLinkedTreeNode[int]
        was_ls_before = false
        array_tree = newSeq[(string, int)]()
    it = head

    processLine("$ ls", it, parseType, name, size, was_ls_before)
    doAssert parseType == ParseType.ls_command
    doAssert name == ""
    doAssert size == 0
    doAssert was_ls_before == true

    processLine("dir m1", it, parseType, name, size, was_ls_before)
    doAssert parseType == ParseType.folder
    doAssert name == "m1"
    doAssert size == 0
    doAssert was_ls_before == true

    processLine("dir m2", it, parseType, name, size, was_ls_before)
    doAssert parseType == ParseType.folder
    doAssert name == "m2"
    doAssert size == 0
    doAssert was_ls_before == true
    
    trasversePreorder(head, array_tree)
    doAssert array_tree.len() == 3
    doAssert array_tree == @[("home", 0), ("m1", 0), ("m2", 0)]

    processLine("$ cd m1", it, parseType, name, size, was_ls_before)
    doAssert parseType == ParseType.cd_command
    doAssert name == "m1"
    doAssert size == 0
    doAssert was_ls_before == false

    processLine("$ ls", it, parseType, name, size, was_ls_before)
    doAssert parseType == ParseType.ls_command
    doAssert name == ""
    doAssert size == 0
    doAssert was_ls_before == true

    processLine("dir mm", it, parseType, name, size, was_ls_before)
    doAssert parseType == ParseType.folder
    doAssert name == "mm"
    doAssert size == 0
    doAssert was_ls_before == true

    array_tree = newSeq[(string, int)]()
    trasversePreorder(it, array_tree)
    doAssert array_tree.len() == 2
    doAssert array_tree == @[("m1", 0), ("mm", 0)]

    array_tree = newSeq[(string, int)]()
    trasversePreorder(head, array_tree)
    doAssert array_tree.len() == 4
    doAssert array_tree == @[("home", 0), ("m1", 0), ("mm", 0), ("m2", 0)]

    doAssert head.name == "home"
    doAssert head.left.name == "m1"
    doAssert head.left.left.name == "mm"
    doAssert head.right.name == "m2"

block CdParent:
    var 
        parseType: ParseType
        name: string = ""
        size: int = 0
        head = newNode[int]("home", 0)
        it: TripleLinkedTreeNode[int]
        was_ls_before = false
        array_tree = newSeq[(string, int)]()
    it = head

    processLine("$ ls", it, parseType, name, size, was_ls_before)
    processLine("dir m1", it, parseType, name, size, was_ls_before)
    processLine("dir m2", it, parseType, name, size, was_ls_before)
    processLine("$ cd m1", it, parseType, name, size, was_ls_before)
    processLine("$ ls", it, parseType, name, size, was_ls_before)
    processLine("dir mm", it, parseType, name, size, was_ls_before)
    processLine("$ cd mm", it, parseType, name, size, was_ls_before)
    processLine("$ ls", it, parseType, name, size, was_ls_before)
    processLine("dir mmm", it, parseType, name, size, was_ls_before)

    array_tree = newSeq[(string, int)]()
    trasversePreorder(head, array_tree)
    doAssert array_tree.len() == 5
    doAssert array_tree == @[("home", 0), ("m1", 0), ("mm", 0), ("mmm", 0), ("m2", 0)]

    doAssert it.name == "mm"

    processLine("$ cd ..", it, parseType, name, size, was_ls_before)
    doAssert parseType == ParseType.cd_command
    doAssert name == ".."
    doAssert size == 0
    doAssert was_ls_before == false

    doAssert it.name == "m1"

    processLine("$ cd ..", it, parseType, name, size, was_ls_before)
    doAssert parseType == ParseType.cd_command
    doAssert name == ".."
    doAssert size == 0
    doAssert was_ls_before == false

    doAssert it.name == "home"

    processLine("$ cd m2", it, parseType, name, size, was_ls_before)
    doAssert parseType == ParseType.cd_command
    doAssert name == "m2"
    doAssert size == 0
    doAssert was_ls_before == false

    doAssert it.name == "m2"

    processLine("$ ls", it, parseType, name, size, was_ls_before)
    processLine("dir mm2", it, parseType, name, size, was_ls_before)

    array_tree = newSeq[(string, int)]()
    trasversePreorder(head, array_tree)
    doAssert array_tree.len() == 6
    doAssert array_tree == @[("home", 0), ("m1", 0), ("mm", 0), ("mmm", 0), ("m2", 0), ("mm2", 0)]

    # TO DO test dir and file with ls
    # TO DO test propagation of size