import std/options

type BufferPayload[T] = object
  cap: int
  data: UncheckedArray[T]

proc sizeOfBufferPayload[T](cap: int): int =
  # align(sizeof(int), alignof(T)) + cap * sizeof(T)
  result = sizeof(int) + cap * sizeof(T)
  let align = alignof(int)
  let `mod` = result mod align
  if `mod` > 0:
    result = result + (align - `mod`)

type Buffer*[T] = object
  p: ptr BufferPayload[T]
  rc, wc: int

proc `=destroy`[T](buf: Buffer[T]) =
  if buf.p != nil:
    dealloc(buf.p)

proc `=copy`[T](
  dst: var Buffer[T], src: Buffer[T]
) {.error: "Buffer[T] cannot be copied".}

proc createBufferPayload[T](cap: int): ptr BufferPayload[T] =
  result = cast[ptr BufferPayload[T]](alloc0(sizeOfBufferPayload[T](cap)))
  result.cap = cap

proc initBuffer*[T](cap: int): Buffer[T] =
  result.p = createBufferPayload[T](cap)

proc cap*[T](buf: Buffer[T]): int =
  buf.p.cap

proc len*[T](buf: Buffer[T]): int =
  buf.wc - buf.rc

proc rrc[T](buf: Buffer[T]): int =
  buf.rc mod buf.cap

proc rwc[T](buf: Buffer[T]): int =
  buf.wc mod buf.cap

proc isEmpty*[T](buf: Buffer[T]): bool =
  buf.len == 0

proc isFull*[T](buf: Buffer[T]): bool =
  buf.len == buf.cap

proc read*[T](buf: var Buffer[T]): Option[T] =
  if buf.isEmpty:
    return none(T)
  result = some(buf.p.data[buf.rrc])
  inc buf.rc

proc write*[T](buf: var Buffer[T], value: sink T) =
  if buf.isFull:
    raise newException(ValueError, "WriteErr: buffer is full")
  buf.p.data[buf.rwc] = value
  inc buf.wc

##############
## operator ##
##############

proc `<<`*[T](buf: var Buffer[T], value: sink T) =
  buf.write(value)

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
