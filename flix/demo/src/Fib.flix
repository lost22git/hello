mod Fib {
  pub def fib(n: Int64): Int64 =
    match n {
      case 0i64 => 1i64
      case 1i64 => 1i64
      case _ => fib(n - 1i64) + fib(n - 2i64)
    }
}
