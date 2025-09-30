#!/usr/bin/env python

# check Tail Call Optimization support


def fib_tail_recur(n: int) -> int:
    """
    fib (tail recur version)

    this would check if TCO
    """

    def visit(i: int, a: int, b: int):
        if i == 0:
            return a
        return visit(i - 1, a + b, a)

    return visit(n, 1, 0)


def fib_recur(n: int) -> int:
    """
    fib (recur version)
    """
    if n <= 1:
        return 1
    return fib_recur(n - 1) + fib_recur(n - 2)


def fib(n: int) -> int:
    """
    fib (iteration version)
    """
    a, b = 1, 1
    for _ in range(1, n):
        a, b = a + b, a

    return a


n = 1111
print(f"fib({n})={fib(n)}")
print(f"fib_tail_recur({n})={fib_tail_recur(n)}")
print(f"fib_recur({n})={fib_recur(n)}")
