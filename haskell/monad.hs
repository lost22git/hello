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
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

main = do
  putStrLn "=== run_guess_num ==="
  run_guess_num
  putStrLn "=== findRightTriangles ==="
  print $ findRightTriangles 100
  putStrLn "=== run_gcd Writer ==="
  print $ runWriter $ gcd' 42 24
  putStrLn "=== run_gcd WriterT ==="
  (runWriterT $ gcd'T 42 24) >>= print
  putStrLn "=== State ==="
  print $ runState ((liftA2 (++) pop pop) >>= push) ["foo", "bar"]
  putStrLn "=== StateT ==="
  runStateT ((liftA2 (++) popT popT) >>= pushT) ["foo", "bar"] >>= print

run_guess_num = do
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


-- Writer Monad
--
-- newtype Writer w a = Writer {runWriter :: (a, w)}
-- instance (Monoid w) => Monad (Writer w) where
--    return a = Writer (a, mempty)
--    Writer (a, w) >>= f =
--        Writer (b, w <> v)
--        where Writer (b, v) = f a
--
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
    tell [show (a, b)]
    return a
  | otherwise = do
    tell [show (a, b)]
    gcd' b (a `mod` b)

-- WriterT
--
-- newtype WriterT w m a = WriterT {runWriterT :: m (a w)}
-- instance (Monoid w, Monad m) => Monad (WriterT w m) where
--    return a = WriterT $ return (a, mempty)
--    WriterT ma >>= f = WriterT $ do
--    (a, w) <- ma
--    (b, v) <- runWriterT (f a)
--    return (b, w <> v)
-- instance (Monoid w) => MonadTrans (WriterT w) where
--    lift :: m a -> WriterT w m a
--    lift ma = WriterT (ma >>= \a -> return (a, mempty))
--

gcd'T :: Int -> Int -> WriterT [String] IO Int
gcd'T a b
  | b == 0 = do
    liftIO $ print (a, b)
    return a
  | otherwise = do
    liftIO $ print (a, b)
    gcd'T b (a `mod` b)

-- State Monad
--
-- newtype State s a = State {runState :: s -> (a,s)}
-- instance Monad (State s) where
--   return a = State $ \s -> (a, s)
--   State m >>= k = State $ \s ->
--     let (a, s1) = m s
--         State m' = k a
--     in m' s1

pop :: State [String] String
pop = do 
  s <- get
  let (x:xs) = s
  put xs
  return x

push :: String -> State [String] String
push a = state $ \s -> ("", (a : s))

-- StateT
--
-- newtype StateT s m a = StateT {runStateT :: s -> m (a,s)}
-- instance (Monad m) => Monad (StateT s m) where
--   return a = StateT $ \s -> return (a, s)
--   StateT g >>= f = StateT $ \s -> do
--                       (a, s') <- g s
--                       runStateT (f a) s'
-- instance (Monad m) => MonadTrans (StateT s m) where
--   lift m = StateT $ \s -> do
--               a <- m
--               return (a, s)

popT :: StateT [String] IO String
popT = do
  s <- get
  let (x:xs) = s
  liftIO $ putStrLn $ "popT state: " ++ (show s) ++ " value: " ++ (show x)
  put xs
  return x

pushT :: String -> StateT [String] IO String
pushT a = do
  liftIO $ putStrLn $ "pushT value: " ++ (show a)
  state $ \s -> ("", (a : s))

-- Other Monads
-- Reader / ReaderT
-- Maybe / MaybeT
-- Except / ExceptT
-- ...
--
-- Classes
-- MonadState
-- MonadReader
-- MonadWriter
-- MonadTrans
-- MonadIO
-- ...
