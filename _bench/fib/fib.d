import std.stdio;

ulong fib(ulong n) {
  if (n < 2) {
    return 1;
  } else {
    return fib(n - 1) + fib(n - 2);
  }
}

void main() {
  ulong n = 40;
  writeln(fib(n));
}
