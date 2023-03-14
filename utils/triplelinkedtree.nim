# Copyright (C) 2023 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

type
    Number = int | float | uint
    Side = enum
        lchild, rlink, both
    TreeError = enum
        treeOk, treeNodeNotFound
    # Triple Linked Binary Tree
    TripleLinkedTreeNode*[T] = ref object
        name*: string
        data*: T
        right*: TripleLinkedTreeNode[T]
        left*: TripleLinkedTreeNode[T]
        parent*: TripleLinkedTreeNode[T]

proc newNode*[T](name: string, data: T): TripleLinkedTreeNode[T] =
    result = TripleLinkedTreeNode[T](name: name, data: data, right: nil, left: nil, parent: nil)

proc push_left*[T](parent: var TripleLinkedTreeNode[T], node: TripleLinkedTreeNode[T]) =
    if node == nil or parent == nil:
        return
    elif parent.left == nil and node.parent == nil:
        parent.left = node
        node.parent = parent

proc push_right*[T](parent: var TripleLinkedTreeNode[T], node: TripleLinkedTreeNode[T]) =
    if node == nil or parent == nil:
        return
    elif parent.right == nil and node.parent == nil:
        parent.right = node
        node.parent = parent

proc addChild*[T](parent: var TripleLinkedTreeNode[T], name: string, data: T, option: Side) =
    case option:
    of lchild, both:
        if parent.left == nil:
            push_left(parent, newNode(name, data))
        elif parent.right == nil:
            push_right(parent, newNode(name, data))
        else:
            addChild(parent.right, name, data, rlink)
    of rlink:
        if parent.right == nil:
            push_right(parent, newNode(name, data))
        else:
            addChild(parent.right, name, data, rlink)

proc addChild*[T](parent: var TripleLinkedTreeNode[T], name: string, data: T) =
    addChild(parent, name, data, both)

proc toDirectly*[T](parent: TripleLinkedTreeNode[T], res: var TripleLinkedTreeNode[T], name: string, option: Side): TreeError =
    if parent == nil:
        return TreeError.treeNodeNotFound
    
    case option:
    of lchild, both:
        if parent.left != nil and parent.left.name == name:
            res = parent.left
            return TreeError.treeOk
        elif parent.right != nil and parent.right.name == name:
            res = parent.right
            return TreeError.treeOk
        else:
            toDirectly(parent.right, res, name, rlink)
    of rlink:
        if parent.right != nil and parent.right.name == name:
            res = parent.right
            return TreeError.treeOk
        else:
            toDirectly(parent.right, res, name, rlink)

proc toDirectly*[T](parent: TripleLinkedTreeNode[T], res: var TripleLinkedTreeNode[T], name: string): TreeError =
    return toDirectly(parent, res, name, both)

proc trasversePreorder*[T](parent: TripleLinkedTreeNode[T], array: var seq[(string, T)]) =
    var it = parent
    if it == nil:
        return
    else:
        array.add((it.name, it.data))
        trasversePreorder(it.left, array)
        trasversePreorder(it.right, array)

proc trasversePreorderData*[T](parent: TripleLinkedTreeNode[T], array: var seq[T]) =
    var it = parent
    if it == nil:
        return
    else:
        array.add(it.data)
        trasversePreorderData(it.left, array)
        trasversePreorderData(it.right, array)

proc trasversePreorderName*[T](parent: TripleLinkedTreeNode[T], array: var seq[string]) =
    var it = parent
    if it == nil:
        return
    else:
        array.add(it.name)
        trasversePreorderName(it.left, array)
        trasversePreorderName(it.right, array)

proc sumPostorder*[T : Number](parent: var TripleLinkedTreeNode[T]) =
    var it = parent
    if it == nil:
        return
    else:
        sumPostorder(it.left)
        sumPostorder(it.right)
        if it.left != nil: it.data += it.left.data
        if it.right != nil: it.data += it.right.data

when isMainModule:
    var t = newNode[int]("\\", 0)
    var it: TripleLinkedTreeNode[int]
    t.addChild("a", 1)
    t.addChild("b", 2)
    t.addChild("c", 3)
    t.addChild("d", 4)
    let o1 = t.toDirectly(it, "a")
    echo o1
    it.addChild("e", 5)
    it.addChild("f", 6)
    it.addChild("g", 7)
    it.addChild("h", 8)
    let o2 = it.toDirectly(it, "e")
    echo o2
    it.addChild("i", 9)
    let o3 = t.toDirectly(it, "d")
    echo o3
    it.addChild("j", 10)
    it.addChild("d1", 11)
    it.addChild("d2", 12)
    it.addChild("k", 13)

    var arr = newSeq[(string, int)]()
    trasversePreorder(t, arr)
    echo arr