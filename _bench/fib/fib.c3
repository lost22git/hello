import std::io;

fn void main() {
  ulong n = 40;
  io::printn(fib(n));
}

fn ulong fib(ulong n) {
  if (n < 2) {
    return 1;
  } else {
    return fib(n-1) + fib(n-2);
  }
}
