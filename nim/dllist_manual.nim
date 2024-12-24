#[
  Doubly Linked List (mm: none)
]#

import std/options

proc create[T](value: sink T): ptr T {.inline, nodestroy.} =
  result = create(T)
  result[] = value

type Node[T] = object
  inner: T
  next, prev: ptr Node[T]

type DLList[T] = object
  head, tail: ptr Node[T]
  len*: int = 0

proc `=destroy`[T](list: DLList[T]) =
  var cc = list.head
  while cc != nil:
    let next = cc[].next
    `=destroy`(cc[].inner)
    dealloc(cc)
    cc = next

proc `=copy`[T](
  dst: var DLList[T], src: DLList[T]
) {.error: "DLList[T] cannot be copied".}

proc initDLList*[T](): DLList[T] =
  DLList[T]()

proc isEmpty*[T](list: DLList[T]): bool =
  list.len == 0

##############
## iterator ##
##############

iterator items*[T](list: DLList[T]): lent T =
  var cc = list.head
  while cc != nil:
    yield cc[].inner
    cc = cc[].next

iterator mitems*[T](list: var DLList[T]): var T =
  var cc = list.head
  while cc != nil:
    yield cc[].inner
    cc = cc[].next

#############
## add/del ##
#############

template addAndReturnIfEmpty[T](list: var DLList[T], value: sink T) =
  if list.isEmpty:
    list.head = Node[T](inner: value).create
    list.tail = list.head
    inc list.len
    return

template delAndReturnIfEmptyOrSingle[T](list: var DLList[T]) =
  if list.isEmpty:
    return none(T)
  if list.len == 1:
    dec list.len
    let n = list.head
    list.head = nil
    list.tail = nil
    let inner = n[].inner
    dealloc(n)
    return some(inner)

proc addHead*[T](list: var DLList[T], value: sink T) =
  addAndReturnIfEmpty(list, value)
  assert list.len > 0
  inc list.len
  let
    oldHead = list.head
    newHead = Node[T](inner: value, next: oldHead).create
  oldHead[].prev = newHead
  list.head = newHead

proc delHead*[T](list: var DLList[T]): Option[T] =
  delAndReturnIfEmptyOrSingle(list)
  assert list.len > 1
  dec list.len
  let
    oldHead = list.head
    newHead = oldHead[].next
  oldHead[].next = nil
  newHead[].prev = nil
  list.head = newHead
  let inner = oldHead[].inner
  dealloc(oldHead)
  return some(inner)

proc addTail*[T](list: var DLList[T], value: sink T) =
  addAndReturnIfEmpty(list, value)
  assert list.len > 0
  inc list.len
  let
    oldTail = list.tail
    newTail = Node[T](inner: value, prev: oldTail).create
  oldTail[].next = newTail
  list.tail = newTail

proc delTail*[T](list: var DLList[T]): Option[T] =
  delAndReturnIfEmptyOrSingle(list)
  assert list.len > 1
  dec list.len
  let
    oldTail = list.tail
    newTail = oldTail[].prev
  oldTail[].prev = nil
  newTail[].next = nil
  list.tail = newTail
  let inner = oldTail[].inner
  dealloc(oldTail)
  return some(inner)

##############
## operator ##
##############

proc `<<|`*[T](list: var DLList[T], value: sink T) {.inline.} =
  list.addHead(value)

proc `>>`*[T](value: sink T, list: var DLList[T]) {.inline.} =
  list.addHead(value)

proc `<<`*[T](list: var DLList[T], value: sink T) {.inline.} =
  list.addTail(value)

proc `<<`*[T](a: var DLList[T], b: sink DLList[T]) =
  if b.isEmpty:
    return
  elif a.isEmpty:
    a = b
  else:
    a.len += b.len
    let oldTail = a.tail
    oldTail[].next = b.head
    b.head[].prev = oldTail
    a.tail = b.tail
    `=wasMoved`(b)

#############
## testing ##
#############

var list = initDLList[string]()
doAssert list.len == 0
doAssert list.isEmpty

doAssert list.delHead == none(string)
doAssert list.delTail == none(string)

list << "foo"
doAssert list.len == 1
list << "bar"
doAssert list.len == 2
"halo" >> list
doAssert list.len == 3

# halo foo bar
for v in list:
  echo "v: ", v

# halo! foo! bar!
for v in list.mitems:
  v = v & "!"
  echo "v: ", v

doAssert list.delHead == "halo!".some
doAssert list.len == 2
doAssert list.delTail == "bar!".some
doAssert list.len == 1
doAssert list.delHead == "foo!".some
doAssert list.len == 0
doAssert list.delTail == none(string)

var a = initDLList[string]()
var b = initDLList[string]()

# a is empty
b << "foo"
a << move b
doAssert a.len == 1
doAssert b.head == nil and b.tail == nil
doAssert b.len == 0
doAssert a.delHead == "foo".some

# b is empty
a << "FOO"
a << move b
doAssert a.len == 1
doAssert a.delHead == "FOO".some

# a,b are not empty
b << "foo"
b << "bar"
a << move b
b << "halo"
b << move a
doAssert a.len == 0
doAssert b.len == 3
doAssert b.delHead == "halo".some
doAssert b.delHead == "foo".some
doAssert b.delHead == "bar".some
doAssert b.delHead == none(string)
