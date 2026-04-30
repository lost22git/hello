main :: IO ()
main = do
  let n = 40
  print $ fib n

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

