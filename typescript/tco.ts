#!/usr/bin/env node

// Check Tail Call Optimization support

function fib_tail_recur(n: number): number {
  function visit(n: number, a: number, b: number) {
    if (n == 0) {
      return a;
    } else {
      return visit(n - 1, a + b, a);
    }
  }
  return visit(n, 1, 0);
}

function fib_recur(n: number): number {
  if (n <= 1) {
    return 1;
  } else {
    return fib_recur(n - 1) + fib_recur(n - 2);
  }
}

function fib_iterate(n: number): number {
  let data = [1, 1];

  for (let i = 1; i < n; i++) {
    data = [data[0] + data[1], data[0]];
  }
  return data[0];
}

let n = 0;
console.log(`fib_tail_recur(${n}) ${fib_tail_recur(n)}`);
n = 1;
console.log(`fib_tail_recur(${n}) ${fib_tail_recur(n)}`);
n = 11;
console.log(`fib_tail_recur(${n}) ${fib_tail_recur(n)}`);
n = 1111;
console.log(`fib_tail_recur(${n}) ${fib_tail_recur(n)}`);

n = 0;
console.log(`fib_iterate(${n}) ${fib_iterate(n)}`);
n = 1;
console.log(`fib_iterate(${n}) ${fib_iterate(n)}`);
n = 11;
console.log(`fib_iterate(${n}) ${fib_iterate(n)}`);
n = 1111;
console.log(`fib_iterate(${n}) ${fib_iterate(n)}`);

n = 0;
console.log(`fib_recur(${n}) ${fib_recur(n)}`);
n = 1;
console.log(`fib_recur(${n}) ${fib_recur(n)}`);
n = 11;
console.log(`fib_recur(${n}) ${fib_recur(n)}`);
n = 1111;
console.log(`fib_recur(${n}) ${fib_recur(n)}`);
