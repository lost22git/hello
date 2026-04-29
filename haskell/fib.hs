import qualified Data.List as List

main = do
  let n = 11
  print $ fib n
  print $ fib_guard n
  print $ fib_case n
  print $ fib' n
  print $ fib'' n

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

fib_guard n
  | n < 0 = error "n must be positive"
  | n < 2 = 1
  | otherwise = fib_guard (n-1) + fib_guard (n-2)

fib_case n = case n of
  0 -> 1
  1 -> 1
  otherwise -> fib_guard (n-1) + fib_guard (n-2)

fib' n = last $ take (n+1) fibs
  where fibs = 1 : 1 : zipWith (+) fibs (drop 1 fibs)

fib'' n = fst $ last $ take (n+1) fibs
  where fibs = List.iterate (\(a, b) -> (b, a + b)) (1, 1)
