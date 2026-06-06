use "collections"

actor Main 
  new create(env: Env) =>
    run_fib(env)
    run_qsort(env)

  fun run_fib(env: Env) =>
    env.out.print("=== run_fib ===")
    let n: U64 = 40
    env.out.print(fib(n).string())

  fun fib(n: U64): U64 =>
    if n < 2 then
      1
    else
      fib(n-1) + fib(n-2)
    end

  fun run_qsort(env: Env) =>
    env.out.print("=== run_qsort ===")
    let a: Array[I64] = [12; 23; 8; 4]
    env.out.print(",".join(a.values()))
    try
      qsort[I64](a, 0, a.size() - 1)?
    else
      env.err.print("ERROR on qsort")
    end
    env.out.print(",".join(a.values()))

  fun qsort[T: Comparable[T] #read](a: Seq[T] ref, l: USize, r: USize) ? =>
    if l >= r then
      return
    end
    let p = partition[T](a, l, r)?
    if p != 0 then
      qsort[T](a, l, p - 1)?
    end
    qsort[T](a, p + 1, r)?

    fun partition[T: Comparable[T] #read](a: Seq[T] ref, l: USize, r: USize): USize ? =>
      var pivot = a(r)?
      var p = l
      for i in Range(l, r) do
        if a(i)? < pivot then
          a(p)? = a(i)? = a(p)?
          p = p + 1
        end
      end
      a(p)? = a(r)? = a(p)?
      p
