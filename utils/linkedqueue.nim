type
    QNode*[T] = ref object
        data: T
        next: QNode[T]
    # Linked FIFO
    LinkedQueue*[T] = ref object
        rear: QNode[T] # insertion from the rear
        front: QNode[T] # deletion from the front

proc newQNode[T](data: T): QNode[T] =
    result = QNode[T](data: data, next: nil)

proc newLinkedQueue*[T](): LinkedQueue[T] =
    result = LinkedQueue[T](rear: nil, front: nil)

proc newLinkedQueue*[T](data: T): LinkedQueue[T] =
    let node = newQNode(data)
    result = LinkedQueue[T](rear: node, front: node)

proc push*[T](queue: var LinkedQueue[T], node: QNode[T]) =
    if node == nil:
        return
    if queue.rear == nil and queue.front == nil:
        queue.rear = node
        queue.front = node
    else:
        queue.rear.next = node
        queue.rear = node

proc push*[T](queue: var LinkedQueue[T], data: T) =
    push(queue, newQNode(data))

proc pop*[T](queue: var LinkedQueue[T]) =
    if queue == nil:
        return
    if queue.front == nil:
        return

    var it = queue.front
    queue.front = it.next
    `=destroy`(it)

    if queue.front == nil:
        queue.rear = nil

proc `$`*[T](node: QNode[T]): string =
    result = ""
    var it = node
    while it != nil:
        result &= $it.value
        result &= "->"
        it = it.next

proc `$`*[T](queue: LinkedQueue[T]): string =
    result = "F-"
    
    if queue != nil:
        var it = queue.front
        while it != nil:
            result &= $it.data
            result &= "-"
            it = it.next
    result &= "R"

proc contains*[T](queue: LinkedQueue[T], data: T): bool =
    if queue == nil:
        return
    var it = queue.front
    result = false
    while it != nil and not result:
        result = it.data == data
        it = it.next

when isMainModule:
    var q = newLinkedQueue[int]()
    echo `$`(q)
    q.push(1)
    echo `$`(q)
    q.push(2)
    echo `$`(q)
    q.push(0)
    echo `$`(q)
    q.pop()
    echo `$`(q)
    q.pop()
    echo `$`(q)
    q.pop()
    echo `$`(q)
    q.pop()
    echo `$`(q)