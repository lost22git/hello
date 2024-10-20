import std/options

type Buffer*[T] = object
  data: ptr UncheckedArray[T]
  cap, len: int
  head, tail: int

proc `=destroy`[T](buf: Buffer[T]) =
  if buf.data != nil:
    for i in (buf.head ..< buf.tail):
      `=destroy`(buf.data[i])
    dealloc(buf.data)

proc `=copy`[T](
  dst: var Buffer[T], src: Buffer[T]
) {.error: "Buffer[T] cannot be copied".}

proc initBuffer*[T](cap: int): Buffer[T] {.inline.} =
  result.cap = cap
  result.data = cast[ptr UncheckedArray[T]](alloc0(cap * sizeof(T)))

proc isEmpty*[T](buf: Buffer[T]): bool {.inline.} =
  buf.len == 0

proc isFull*[T](buf: Buffer[T]): bool {.inline.} =
  buf.len == buf.cap

proc remToAdd*[T](buf: Buffer[T]): int {.inline.} =
  buf.cap - buf.len

proc delHead*[T](buf: var Buffer[T]): Option[T] =
  if buf.isEmpty:
    return none(T)
  let head = buf.head
  result = some(buf.data[head])
  buf.head = (head + 1) mod buf.cap
  dec buf.len

proc addHead*[T](buf: var Buffer[T], value: sink T) =
  if buf.isFull:
    raise newException(ValueError, "addHead: buffer is full")
  let head = (buf.head + buf.cap - 1) mod buf.cap
  buf.data[head] = value
  buf.head = head
  inc buf.len

proc delTail*[T](buf: var Buffer[T]): Option[T] =
  if buf.isEmpty:
    return none(T)
  let tail = (buf.tail + buf.cap - 1) mod buf.cap
  result = some(buf.data[tail])
  buf.tail = tail
  dec buf.len

proc addTail*[T](buf: var Buffer[T], value: sink T) =
  if buf.isFull:
    raise newException(ValueError, "addTail: buffer is full")
  let tail = buf.tail
  buf.data[tail] = value
  buf.tail = (tail + 1) mod buf.cap
  inc buf.len

################
## read/write ##
################

proc read*[T](buf: var Buffer[T]): Option[T] {.inline.} =
  buf.delHead()

proc write*[T](buf: var BUffer[T], value: sink T) {.inline.} =
  buf.addTail()

##############
## operator ##
##############

proc `<<`*[T](buf: var Buffer[T], value: sink T) {.inline.} =
  buf.addTail(value)

proc `<<|`*[T](list: var Buffer[T], value: sink T) {.inline.} =
  list.addHead(value)

proc `>>`*[T](value: sink T, list: var Buffer[T]) {.inline.} =
  list.addHead(value)

#############
## testing ##
#############

var buf = initBuffer[string](cap = 3)
doAssert buf.len == 0
doAssert buf.cap == 3
doAssert buf.read == none(string)

buf << "halo"
buf << "foo"
buf << "bar"
doAssertRaises(ValueError):
  buf << "zzz"
doAssert buf.len == 3
doAssert buf.read == "halo".some
doAssert buf.read == "foo".some
doAssert buf.read == "bar".some
doAssert buf.read == none(string)
doAssert buf.len == 0

"halo" >> buf
"foo" >> buf
buf << "bar"

doAssertRaises(ValueError):
  "zzz" >> buf

doAssert buf.len == 3
doAssert buf.delTail == "bar".some
doAssert buf.delHead == "foo".some
doAssert buf.delTail == "halo".some
doAssert buf.delTail == none(string)
