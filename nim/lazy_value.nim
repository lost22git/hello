import std/locks, os

type LazyValue[T, A] = object
  lock: Lock
  completed: bool
  value: T
  loader: proc(arg: A): T
  loaderArg: A

proc `=destroy`[T, A](lazyValue: var LazyValue[T, A]) =
  echo "destroy is calling"
  deinitLock(lazyValue.lock)

proc initLazyValue[T, A](loader: proc(arg: A): T, loaderArg: A): LazyValue[T, A] =
  result = LazyValue[T, A](loader: loader, loaderArg: loaderArg)
  initLock(result.lock)

proc get[T, A](lazyValue: var LazyValue[T, A]): T =
  if lazyValue.completed:
    return lazyValue.value

  withLock lazyValue.lock:
    if lazyValue.completed:
      return lazyValue.value
    lazyValue.value = lazyValue.loader(lazyValue.loaderArg)
    lazyValue.completed = true
    return lazyValue.value

# === Test ===

proc load(n: int): int =
  echo "loading n=" & $n
  sleep 1000
  n * 10

var lzv = initLazyValue[int, int](load, 11)

proc work(i: int) {.thread.} =
  {.gcsafe.}:
    let v = lzv.get()
    echo $i & " -> " & $v

var ts = newSeq[Thread[int]](10)

for i in 0 .. ts.high:
  createThread(ts[i], work, i)

joinThreads ts

echo "end"
