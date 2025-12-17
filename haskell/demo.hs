#!/usr/bin/env -S ghc --run

import Text.Printf
import System.IO
import Control.Exception
import Data.Monoid
import Control.Concurrent
import Control.DeepSeq
import Data.Function ((&))

main = do
  testSort
  testMail
  testPipe
  testFunctor
  testMonoid
  testExceptionHandle
  testExceptionCatch
  testExceptionTry
  testMVar
  testChan
  testFib

-- === let vs where ===

testSort = 
  let 
    a = [1,19,34,4,3]
  in
    printf "%s sorted: %s\n" (show a) $ show $ sort a
  
sort [] = []
sort (x:xs) = sort ys ++ [x] ++ sort zs
  where
    ys = [a | a<-xs, a<x]
    zs = [a | a<-xs, a>=x]

-- sort (x:xs) = 
--   let
--     ys = [a | a<-xs, a<x]
--     zs = [a | a<-xs, a>=x]
--   in
--     sort ys ++ [x] ++ sort zs
 

-- === ADT & Pattern Matching ===

testMail = 
  let 
    name = "haskell"
    provider = Tuta 
  in
    printf "mail address generated: %s\n" $ mailAddr name provider

data MailProvider = Tuta | Proton

mailAddr :: String -> MailProvider -> String
mailAddr name provider = name ++ "@" ++ mailSuffix provider

mailSuffix :: MailProvider -> String
mailSuffix Tuta = ["tutamail.com", "tuta.io"] !! 1
mailSuffix Proton = ["protonmail.com", "proton.me"] !! 1


-- === Pipe ===

testPipe = do
  printf "pipe $: %d\n" $ product $ fmap (\c -> read [c] ::Int) "237"
  "237" & fmap (\c -> read [c] :: Int) & product & printf "pipe &: %d\n"

-- === Functor ===

-- ```
-- class Functor f where
--   fmap :: (a->b) -> f a ->  f b
-- ```

testFunctor = do
  printf "compose functions via .: %d\n" $ (*2) . (*3) $ 7
  printf "compose functions via functor fmap: %d\n" $ fmap (*2) (*3) $ 7
  printf "compose functions via functor fmap <$>: %d\n" $ (*2) <$> (*3) $ 7
 
-- === Monoids ===

-- ```
-- class Monoids m where
--   mempty :: m
--   mappend :: m -> m -> m
-- ```

testMonoid = do
  printf "compose functions via monoid mappend: %d\n" $ appEndo (mappend (Endo (*2)) (Endo (*3))) 7
  printf "compose functions via monoid mappend <>: %d\n" $ appEndo (Endo (*2) <> Endo (*3)) 7


-- === Monad ===

-- ```
-- class Monad m where
--   return :: a -> m a
--   (>>=) :: (a->m b) -> m a -> m b
-- ```

testMonad = do
  -- Monoad for function
  -- f >>= g = \x -> g (f x) x
  assert (((+2) >>= (*) $ 3) == ((3+2) * 3)) 

  -- Applicative for function
  -- f <*> g = \x -> f (g x) x
  assert (((*) <*> (+2) $ 3) == ((3+2) * 3))

-- === IO ===

testFileIO path = 
  withFile path ReadMode $ \file -> do
  content <- hGetContents file
  return $! force content

-- === Exception Handling ===
-- handle
testExceptionHandle = handle
  (\exn -> printf "Error: %s\n" $ show (exn :: IOException))
  (testFileIO "README2.md" >>= \content -> do
    putStrLn "README.md content:"
    putStrLn content)

-- catch
testExceptionCatch = catch
  (testFileIO "README2.md" >>= \content -> do
    putStrLn "README.md content:"
    putStrLn content)
  (\exn -> printf "Error: %s\n" $ show (exn :: IOException))
  
-- try
testExceptionTry = do
  result <- try (testFileIO "README2.md")
  case result of 
    Left exn -> printf "Error: %s\n" $ show (exn :: IOException)
    Right content -> do
      putStrLn "README.md content:"
      putStrLn content

-- === Concurrent ===

-- - Control.Concurrent.MVar => SynchronousQueue
-- - Control.Concurrent.TVar => STM
-- - Control.Concurrent.Chan => Channel/BlockingQueue

testMVar = do
    mv <- newEmptyMVar
    _ <- forkIO $ do
        threadId <- myThreadId
        printf "[%s] MVar sent: 42\n" $ show threadId
        putMVar mv 42
    v <- takeMVar mv
    putStrLn $ "[main] MVar received: " ++ show v

testChan = do
    ch <- newChan
    _ <- forkIO $ do
        threadId <- myThreadId
        printf "[%s] Chan sent: 42\n" $ show threadId
        writeChan ch 42
    v <- readChan ch
    putStrLn $ "[main] Chan received: " ++ show v


-- === lazy evaluation ===

fib n = last $ take (n+1) fibs
  where fibs = 1 : 1 : zipWith (+) fibs (drop 1 fibs)

testFib = do
  printf "fib 0 = %d\n" $ fib 0
  printf "fib 1 = %d\n" $ fib 1
  printf "fib 11 = %d\n" $ fib 11
  printf "fib 111 = %d\n" $ fib 111
  
