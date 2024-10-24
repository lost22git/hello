proc printf(fmt: cstring): cint {.importc, varargs, discardable.}
printf("printf: hello %s\n", "c")

{.
  emit:
    """
static int add(int a, int b) {
  return a + b;
}
"""
.}
proc add(a, b: cint): cint {.importc.}
echo "add(1,2) => ", add(1, 2)
