import std/[strformat, cpuinfo, typedthreads, asyncdispatch]

proc slowWork(i: int) {.async.} =
  echo fmt"slowWork {i=} start"
  await sleepAsync 30000
  echo fmt"slowWork {i=} end"

proc run(i: int) =
  echo fmt "run {i=} start"
  waitFor slowWork i
  echo fmt "run {i=} end"

let nthreads = countProcessors()
var threads = newSeq[Thread[int]] nthreads

for i in 0 .. threads.high:
  createThread threads[i], run, i

joinThreads threads
