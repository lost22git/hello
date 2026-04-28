fn fib(n: usize) -> usize {
    if n < 2 {
        return 1;
    } else {
        return fib(n - 1) + fib(n - 2);
    }
}

fn main() {
    let n = 40;
    println!("{}", fib(n));
}
