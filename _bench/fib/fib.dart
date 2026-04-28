int fib(n) {
  if (n < 2) {
    return 1;
  } else {
    return fib(n - 1) + fib(n - 2);
  }
}

void main(List<String> args) {
  int n = 40;
  print(fib(n));
}
