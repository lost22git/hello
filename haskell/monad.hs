-- Monad
--
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--
--   (>>)  :: m a -> m b -> m b
--   >> = *> -- from Applicative
--
--   return :: a -> m a
--   return = pure -- from Applicative


import Text.Read (readMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)

main = do
  test_guess_num
  print $ findRightTriangles 100

test_guess_num = do
  n <- getPOSIXTime >>= (pure . (`mod` 100) . floor)
  guess_num n

guess_num n = do
  putStr "\27[33mInput a number: \27[m"
  s <- getLine
  case (readMaybe s) of
    Just n' | n' == n ->
      putStrLn "\27[32mYou Win!\27[m"
    Just n' | n' < n -> do
      putStrLn "\27[31mYou Lose!\27[m Too small, try again."
      guess_num n
    Just n' | n' > n -> do
      putStrLn "\27[31mYou Lose!\27[m Too big, try again."
      guess_num n
    Nothing ->
      fail "Input is not a valid number."

findRightTriangles n = do
  a <- [1..n]
  b <- [a..n]
  c <- [b..n]
  if a ^ 2 + b ^ 2 == c ^ 2
  then [(a, b, c)]
  else []
