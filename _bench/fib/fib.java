import java.lang.System.*;

public class fib {
  public static void main(String[] args) {
    var n = 40;
    System.out.printf("%d\n", fib(n));
  }

  static long fib(long n) {
    if (n < 2) {
      return 1l;
    } else {
      return fib(n - 1) + fib(n - 2);
    }
  }
}
