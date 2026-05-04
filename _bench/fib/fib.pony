actor Main 
  new create(env: Env) =>
    let n: U64 = 40
    env.out.print(fib(n).string())

  fun fib(n: U64): U64 =>
    if n < 2 then
      1
    else
      fib(n-1) + fib(n-2)
    end
