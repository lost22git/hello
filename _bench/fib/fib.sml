fun fib 0 = 1
  | fib 1 = 1
  | fib n = fib (n-1) + fib (n-2)

fun main () = 
  let 
    val n = 40
  in 
   print (Int.toString (fib n));
   print "\n"
  end
