import std/options

type BufferPayload[T] = object
  cap: int
  data: UncheckedArray[T]

# Note:
# current implementaion is based on the assumpation: align = 8B
proc sizeOfBufferPayload[T](cap: int): int =
  result = sizeof(int) + cap * sizeof(T)
  let align = alignof(int)
  let `mod` = result mod align
  if `mod` > 0:
    result = result + (align - `mod`)

type Buffer*[T] = object
  p: ptr BufferPayload[T]
  head, tail: int
  len: int

proc `=destroy`[T](buf: Buffer[T]) =
  if buf.p != nil:
    for i in (buf.head ..< buf.tail):
      `=destroy`(buf.p.data[i])
    dealloc(buf.p)

proc `=copy`[T](
  dst: var Buffer[T], src: Buffer[T]
) {.error: "Buffer[T] cannot be copied".}

proc createBufferPayload[T](cap: int): ptr BufferPayload[T] {.inline.} =
  result = cast[ptr BufferPayload[T]](alloc0(sizeOfBufferPayload[T](cap)))
  result.cap = cap

proc initBuffer*[T](cap: int): Buffer[T] {.inline.} =
  result.p = createBufferPayload[T](cap)

proc cap*[T](buf: Buffer[T]): int {.inline.} =
  buf.p.cap

proc isEmpty*[T](buf: Buffer[T]): bool {.inline.} =
  buf.len == 0

proc isFull*[T](buf: Buffer[T]): bool {.inline.} =
  buf.len == buf.cap

proc delHead*[T](buf: var Buffer[T]): Option[T] =
  if buf.isEmpty:
    return none(T)
  let head = buf.head
  result = some(buf.p.data[head])
  buf.head = (head + 1) mod buf.cap
  dec buf.len

proc addHead*[T](buf: var Buffer[T], value: sink T) =
  if buf.isFull:
    raise newException(ValueError, "addHead: buffer is full")
  let head = (buf.head + buf.cap - 1) mod buf.cap
  buf.p.data[head] = value
  buf.head = head
  inc buf.len

proc delTail*[T](buf: var Buffer[T]): Option[T] =
  if buf.isEmpty:
    return none(T)
  let tail = (buf.tail + buf.cap - 1) mod buf.cap
  result = some(buf.p.data[tail])
  buf.tail = tail
  dec buf.len

proc addTail*[T](buf: var Buffer[T], value: sink T) =
  if buf.isFull:
    raise newException(ValueError, "addTail: buffer is full")
  let tail = buf.tail
  buf.p.data[tail] = value
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

doAssert sizeOfBufferPayload[string](3) == (8 + 3 * 16)

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
