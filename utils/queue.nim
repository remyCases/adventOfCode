type
    Queue*[T] = ref object
        value: T
        next: Queue[T]

proc newNode[T](v: T): Queue[T] =
    result = Queue[T](value: v, next: nil)

proc push*[T](root: var Queue[T], node: Queue[T]) =
    if node == nil:
        return
    if root == nil:
        root = node
    else:
        node.next = root
        root = node

proc push*[T](root: var Queue[T], data: T) =
    push(root, newNode(data))

proc popNoFree*[T](root: var Queue[T]): Queue[T] =
    if root == nil:
        return nil
    else:
        result = root
        root = root.next
        result.next = nil

proc headValue*[T](root: Queue[T]): T =
    if root == nil:
        raise 
    else:
        return root.value

proc separateHead*[T](root: var Queue[T], indexToSeparate: int): Queue[T] =

    result = root
    var currentIndex = 0
    var prev: Queue[T] = nil
    while currentIndex < indexToSeparate:
        if root == nil:
            raise newException(RangeDefect, "Asked index to high, out of queue")
        prev = root
        root = root.next
        currentIndex.inc
    
    prev.next = nil

proc reunite*[T](head: Queue[T], tail: var Queue[T])  =
    if head == nil:
        return
    else:
        var it = head
        while it.next != nil:
            it = it.next
        it.next = tail
        tail = head

proc `$`*[T](root: Queue[T]): string =
    result = ""
    var it = root
    while it != nil:
        result &= $it.value
        result &= "->"
        it = it.next

proc `$`*[T](roots: seq[Queue[T]]): string =
    result = ""
    for root in roots:
        var it = root
        while it != nil:
            result &= $it.value
            result &= "->"
            it = it.next
        result &= "\n"