#!/usr/bin/env -S nim r --mm:atomicArc

import std/[strtabs, math, cpuinfo, sequtils, hashes]
import threading/rwlock

type Shard = ref object
  lock: RwLock
  tab: StringTableRef

proc newShard(mode = modeCaseInsensitive): Shard =
  Shard(lock: createRwLock(), tab: newStringTable(mode))

type DashTable* = ref object
  shards: seq[Shard]
  mode: StringTableMode

let defaultShards* = (max(1, countProcessors()) * 4).nextPowerOfTwo()

proc myhash(table: DashTable, key: string): Hash =
  ## compute hash of `key`
  ## this code is from [source](https://github.com/nim-lang/Nim/blob/version-2-2/lib/pure/strtabs.nim#L117)

  case table.mode
  of modeCaseSensitive:
    result = hashes.hash(key)
  of modeCaseInsensitive:
    result = hashes.hashIgnoreCase(key)
  of modeStyleInsensitive:
    result = hashes.hashIgnoreStyle(key)

# === PUBLIC ITERATORSa ===

iterator pairs*(table: DashTable): tuple[key, value: string] =
  for s in table.shards:
    readWith s.lock:
      for e in pairs(s.tab):
        yield e

iterator keys*(table: DashTable): string =
  for s in table.shards:
    readWith s.lock:
      for k in keys(s.tab):
        yield k

iterator values*(table: DashTable): string =
  for s in table.shards:
    readWith s.lock:
      for v in values(s.tab):
        yield v

# === PUBLIC PROCEDURES ===

proc shardWithIndex(table: DashTable, key: string): (int, Shard) {.inline.} =
  ## compute shards index of `key` and return `(index,shard)`

  let index = table.myhash(key) and table.shards.high
  (index, table.shards[index])

proc hasKey*(table: DashTable, key: string): bool =
  ## check `key` if found

  let (_, shard) = table.shardWithIndex(key)
  readWith shard.lock:
    result = shard.tab.hasKey(key)

proc get*(
    table: DashTable, key: string, f: proc(key: string): string
): string {.effectsOf: f.} =
  ## get the value mapped to `key`
  ## if `key` not found, call `f` and return the result
  ##
  ## - raise error if error raised on calling `f`

  let (_, shard) = table.shardWithIndex(key)
  readWith shard.lock:
    if shard.tab.hasKey(key):
      return shard.tab[key]

  # key not found
  return f(key)

proc get*(table: DashTable, key: string): string {.raises: [KeyError].} =
  ## get the value mapped to `key`
  ##
  ## - raise error if `key` not found

  get(table, key) do(key: string) -> string:
    raise newException(KeyError, "key=" & key)

proc get*(table: DashTable, key, fallbackValue: sink string): string =
  ## get the value mapped to `key`
  ## if `key` not found, return `fallbackValue`

  get(table, key) do(key: string) -> string:
    fallbackValue

proc get*(
    table: DashTable, key: string, f: proc(key, value: string)
) {.raises: [KeyError], effectsOf: f.} =
  ## get the value mapped to `key` and call `f`
  ##
  ## - raise error if `key` not found
  ## - raise error if error raised on calling `f`
  ## - `f` is called in readlock block

  let (_, shard) = table.shardWithIndex(key)
  readWith shard.lock:
    if shard.tab.hasKey(key):
      let value = shard.tab[key]
      f(key, value)
      return

  # key not found
  raise newException(KeyError, "key=" & key)

proc add*(table: DashTable, key, value: sink string) =
  ## add `key`-`value` entry

  let (_, shard) = table.shardWithIndex(key)
  writeWith shard.lock:
    shard.tab[key] = value

proc del*(table: DashTable, key: string) =
  ## delete the associated entry of `key` (ignore `key` not found)

  # key not found
  if not table.hasKey(key):
    return

  let (_, shard) = table.shardWithIndex(key)
  writeWith shard.lock:
    shard.tab.del(key)

proc take*(
    table: DashTable, key: string, f: proc(key: string): string
): string {.effectsOf: f.} =
  ## take the value mapped to `key` and delete the associated entry
  ## if `key` not found, call `f` and return the result
  ##
  ## - raise error if error raised on calling `f`

  # key not found
  if not table.hasKey(key):
    return f(key)

  let (_, shard) = table.shardWithIndex(key)
  writeWith shard.lock:
    if shard.tab.hasKey(key):
      result = shard.tab[key]
      shard.tab.del(key)
      return

  # key not found
  return f(key)

proc take*(table: DashTable, key: string): string {.raises: [KeyError].} =
  ## take the value mapped to `key` and delete the associated entry
  ##
  ## - raise error if `key` not found

  take(table, key) do(key: string) -> string:
    raise newException(KeyError, "key=" & key)

proc getOrAdd*(
    table: DashTable, key: sink string, f: proc(key: string): string
): string {.effectsOf: f.} =
  ## get the value mapped to `key`
  ## if `key` not found, add `key`-`value`(the result of calling `f`) entry first
  ##
  ## - raise error if error raised on calling `f`
  ## - `f` is called on writelock block

  try:
    return table.get(key)
  except KeyError:
    discard

  let (_, shard) = table.shardWithIndex(key)
  writeWith shard.lock:
    if shard.tab.hasKey(key):
      return shard.tab[key]
    else:
      let value = f(key)
      shard.tab[key] = value
      return value

proc getOrAdd*(table: DashTable, key, valueToAddOnKeyNotFound: sink string): string =
  ## get the value mapped to `key`
  ## if `key` not found, add `key`-`valueToAddOnKeyNotFound` entry

  getOrAdd(table, key) do(key: string) -> string:
    valueToAddOnKeyNotFound

proc update*(
    table: DashTable, key: string, f: proc(key: string, value: var string)
) {.raises: [KeyError], effectsOf: f.} =
  ## update the value mapped to `key` via calling `f`
  ##
  ## - raise error if `key` not found
  ## - raise error if error raised on calling `f`
  ## - `f` is called on writelock block

  # key not found
  if not table.hasKey(key):
    raise newException(KeyError, "key=" & key)

  let (_, shard) = table.shardWithIndex(key)
  writeWith shard.lock:
    if shard.tab.hasKey(key):
      f(key, shard.tab[key])
      return

  # key not found
  raise newException(KeyError, "key=" & key)

proc len*(table: DashTable): int =
  for s in table.shards:
    readWith s.lock:
      result += s.tab.len

proc isEmpty*(table: DashTable): bool =
  table.len == 0

proc clear*(table: DashTable) =
  for s in table.shards:
    writeWith s.lock:
      s.tab.clear()

# === PUBLIC OPERATORS ===

proc `[]`*(table: DashTable, key: string): string {.raises: [KeyError].} =
  table.get(key)

proc `[]=`*(table: DashTable, key, value: sink string) =
  table.add(key, value)

# === PUBLIC CONSTRUCTORS ===

proc newDashTable*(
    shards: int = defaultShards, mode: StringTableMode = modeCaseInsensitive
): DashTable =
  ## new `DashTable` instance

  doAssert shards > 1, "shards must be >1"
  doAssert isPowerOfTwo(shards), "shards must be power of 2"

  DashTable(shards: newSeqWith(shards, newShard(mode)), mode: mode)

# === Test ===
# --mm:atomicArc

import std/[strutils]

proc run(f: proc(args: (int, DashTable)) {.thread.}, table: DashTable) =
  var ts = newSeq[Thread[(int, DashTable)]](100)
  for i in 0 .. ts.high:
    createThread(ts[i], f, (i, table))
  joinThreads(ts)

proc getOrAdd(args: (int, DashTable)) {.thread.} =
  let (i, table) = args
  let value = table.getOrAdd($i) do(key: string) -> string:
    $(parseInt(key) * 10)
  echo "getOrAdd:" & $i & "=>" & value

proc update(args: (int, DashTable)) {.thread.} =
  let (i, table) = args
  table.update($i) do(key: string, value: var string):
    value.add "-updated"

proc take(args: (int, DashTable)) {.thread.} =
  let (i, table) = args
  let value = table.take($i)
  echo "take:" & $i & "=>" & value

let table = newDashTable()
echo "=".repeat(44)
run(getOrAdd, table)
run(update, table)
doAssert table.len == 100
echo "=".repeat(44)
for (k, v) in pairs(table):
  echo "pair:" & $k & "=>" & v
echo "=".repeat(44)
run(take, table)
doAssert table.isEmpty
