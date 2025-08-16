import std/[asyncdispatch, tables]

type Loader[K, V] = proc(k: K): Future[V] {.async, closure.}

type Cache[K, V] = ref object
  tab: TableRef[K, V] # table to store key -> value
  loader: Loader[K, V] # loader to load value by key when key not found in tab
  loadingFutTab: TableRef[K, Future[void]] # table to store key -> loadingfuture

proc newCache*[K, V](loader: Loader[K, V]): Cache[K, V] =
  result = Cache[K, V](
    tab: newTable[K, V](), loader: loader, loadingFutTab: newTable[K, Future[void]]()
  )

proc get[K, V](cache: Cache[K, V], key: K): Future[V] {.async.} =
  if cache.tab.contains(key):
    return cache.tab[key]

  if cache.loadingFutTab.contains(key):
    await cache.loadingFutTab[key]
    return await cache.get(key)
  else:
    let loadingFut = newFuture[void]("cache notify future: key=" & $key)
    cache.loadingFutTab[key] = loadingFut
    try:
      let value = await cache.loader(key)
      cache.tab[key] = value
      return value
    finally:
      cache.loadingFutTab.del key
      loadingFut.complete()

proc del[K, V](cache: Cache[K, V], key: K) =
  cache.tab.del key

# === Test ===

var first = true
proc compute(i: int): Future[int] {.async.} =
  echo "call compute with i=", i
  await sleepAsync 2000
  if first:
    first = false
    raise newException(ValueError, "error on first")
  return i * 10

let cache = newCache[int, int](loader = compute)

proc foo(i: int) {.async.} =
  try:
    let v = await cache.get(1)
    echo i, "->", v
  except Exception as e:
    echo i, "->", e.msg

echo "=== begin ==="

var futs: seq[Future[void]] = @[]
for i in 1 .. 3:
  futs.add foo(i)

waitFor all futs

echo "=== again ===="

futs = @[]
for i in 1 .. 3:
  futs.add foo(i)

waitFor all futs

echo "=== end ===="
