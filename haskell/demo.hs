#!/usr/bin/env -S ghc --run

main = do
  putStrLn $ show $ sort xs
  putStrLn $ show $ myReverse xs
  where xs = [1..4] ++ [10,9..5]
  
sort [] = []
sort (x:xs) = 
  sort ys ++ [x] ++ sort zs
  where
    ys = [a | a<-xs, a<x]
    zs = [a | a<-xs, a>=x]

myReverse [] = []
myReverse (x:xs) = 
  (myReverse xs) ++ [x]
