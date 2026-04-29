#!/usr/bin/env runhaskell

import Text.Printf
import Control.Exception
import Data.Monoid

main = do
  testSort
  testMail
  testMonoid

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

-- === Concurrent ===

