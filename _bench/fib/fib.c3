import std::io;

fn void main() {
  int n = 40;
  io::printn(fib(n));
}

fn int fib(int n) {
  if (n < 2) {
    return 1;
  } else {
    return fib(n-1) + fib(n-2);
  }
}
