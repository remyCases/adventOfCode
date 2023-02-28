type
    # Linked FILO
    LinkedNode*[T] = ref object
        value: T
        next: LinkedNode[T]

proc newNode[T](v: T): LinkedNode[T] =
    result = LinkedNode[T](value: v, next: nil)

proc push*[T](root: var LinkedNode[T], node: LinkedNode[T]) =
    if node == nil:
        return
    if root == nil:
        root = node
    else:
        node.next = root
        root = node

proc push*[T](root: var LinkedNode[T], data: T) =
    push(root, newNode(data))

proc pop*[T](root: var LinkedNode[T]) =
    var it = root
    if root != nil:
        root = root.next
    `=destroy`(it)

proc popNoFree*[T](root: var LinkedNode[T]): LinkedNode[T] =
    if root == nil:
        return nil
    else:
        result = root
        root = root.next
        result.next = nil

proc headValue*[T](root: LinkedNode[T]): T =
    if root == nil:
        raise 
    else:
        return root.value

proc separateHead*[T](root: var LinkedNode[T], indexToSeparate: int): LinkedNode[T] =
    result = root
    var currentIndex = 0
    var prev: LinkedNode[T] = nil
    while currentIndex < indexToSeparate:
        if root == nil:
            raise newException(RangeDefect, "Asked index to high, out of LinkedList")
        prev = root
        root = root.next
        currentIndex.inc
    
    prev.next = nil

proc reunite*[T](head: LinkedNode[T], tail: var LinkedNode[T])  =
    if head == nil:
        return
    else:
        var it = head
        while it.next != nil:
            it = it.next
        it.next = tail
        tail = head

proc `$`*[T](root: LinkedNode[T]): string =
    result = ""
    var it = root
    while it != nil:
        result &= $it.value
        result &= "->"
        it = it.next

proc `$`*[T](roots: seq[LinkedNode[T]]): string =
    result = ""
    for root in roots:
        var it = root
        while it != nil:
            result &= $it.value
            result &= "->"
            it = it.next
        result &= "\n"

proc contains*[T](root: LinkedNode[T], value: T): bool =
    var it = root
    result = false
    while it != nil and not result:
        result = it.value == value
        it = it.next