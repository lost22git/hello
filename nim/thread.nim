#[
///usr/bin/env nim r "$0" "$@" ; exit $?
]#

import std/[strformat]
import std/[os]
import std/typedthreads
import std/[monotimes, times]

var threads = newSeq[Thread[void]](10)
# var threads: array[10, Thread[void]]

let st = getMonoTime()

proc doSleep() =
  echo fmt"{getThreadId() = }"
  sleep 3000

for i in 0 ..< threads.high:
  createThread(threads[i], doSleep)

threads.joinThreads

let ed = getMonoTime()

echo fmt"elapsed: {(ed-st).inMilliseconds}ms"
