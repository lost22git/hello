// Check Tail Call Optimization support

// fib: recur variant
int fib_recur(int n) {
  if (n <= 1) return 1;
  return fib_recur(n - 1) + fib_recur(n - 2);
}

// fib: tail recur variant
int fib_tail_recur(int n) {
  return visit(n, 1, 0);
}

int visit(int i, int a, int b) {
  if (i == 0) return a;
  return visit(i - 1, a + b, a);
}

// fib: iterate variant
int fib_iterate(int n) {
  var data = [1, 1];
  for (var i = 1; i < n; i++) {
    data = [data[0] + data[1], data[0]];
  }
  return data[0];
}

main(List<String> args) {
  int n = 1111;
  print('fib_iterate(${n}) = ${fib_iterate(n)}');
  print('fib_tail_recur(${n}) = ${fib_tail_recur(n)}');
  print('fib_recur(${n}) = ${fib_recur(n)}');
}
