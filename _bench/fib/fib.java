import java.lang.System.*;

public class fib {
  public static void main(String[] args) {
    int n = 40;
    System.out.printf("%d\n", fib(n));
  }

  static int fib(int n) {
    if (n < 2) {
      return 1;
    } else {
      return fib(n - 1) + fib(n - 2);
    }
  }
}
