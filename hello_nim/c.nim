{.
  emit:
    """
#include <stdio.h>
static void hello_c() {
  printf("Hello C\n");
}
"""
.}

proc hello_c() {.importc: "hello_c".}

hello_c()
