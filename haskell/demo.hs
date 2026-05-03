#!/usr/bin/env -S ghc --run

import Text.Printf

main = do
  testSort

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
