# TODO

## porting `nsync` to Nim

- https://github.com/google/nsync
- https://github.com/jart/cosmopolitan/tree/master/third_party/nsync
- https://github.com/ysbaddaden/sync

mutex/rwlock = statemanagement(lockstate + CAS) + listeners(Dll + park/unpark)

rwlock = sharedlock + exclusivelock
- multi-threads enter shared region
- single-thread enter exclusive region

reentrantlock = mutex + heldlockthreadid + reentrantcount

condtionvariable = statemanagement(condtionstate + CAS) + listeners(Dll + park/unpark)
- wait: relase mutex, re-aquire mutex (after singaled)

## porting `NonBlockingHashMap` of  `JCtools` to Nim

- https://github.com/JCTools/JCTools.git
- https://github.com/JCTools/JCTools/issues/87

kvs
newkvs

kvs
- 0: CHM 
- 1: hash: int[]
- 2: key0
- 3: value0
- ...

tablefull:
- reprobecount reach limit
- length reach limit

newsize:
- if len >25% of cap then double cap
- if len < 1M and len > 50% of cap then double double cap
- if len >= 1M and len > 75% of cap then double double cap
- if newsize <= oldlen (aka keep or shrink) and lastresize happend 10sec ago then double cap

copysolt:
- oldkey -> TOMBSTONE
- oldValue -> Prime(copying) -> TOMBPRIME(copy_done)

note:
- In the remove operation, only val is removed but the key is not, which may cause the key cannot be gc, unless for subsequent replace or resize

