#!/usr/bin/env -S nim r

import strformat, os, strutils, typetraits

type A = object of RootObj
type B = object of RootObj
type C = object of RootObj

proc foo(x: A) =
  echo fmt"[A]"

proc foo(x: B) =
  echo fmt"[B]"

proc foo(x: C) =
  echo fmt"[C]"

# this would be specialized to test(A) and test(B)
proc test(x: A | B) =
  let t = typeof(x).name()
  echo fmt"[A|B] type of x is {t}"
  foo(x)

# this would be specialized to test(A) and test(B) and test(C)
proc test(x: auto) =
  let t = typeof(x).name()
  echo fmt"[auto] type of x is {t}"
  foo(x)

# this would not be compiled
# same as test(auto)
# proc test[T](x: T) =
#   let t = typeof(x).name()
#   echo fmt"[T] type of x is {t}"
#   foo(x)

# this would not be compiled.
# dispatch of foo is compile-time and foo(RootObj) not found
# proc test(x: RootObj) =
#   let t = typeof(x).name()
#   echo fmt"[RootObj] type of x is {t}"
#   foo(x)

test(A())
test(B())
test(C())
