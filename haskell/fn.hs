import Data.Function

main = do
  test_apply
  test_pipe
  test_compose
  test_id
  test_const
  test_on

div' a b = 
  a / b

test_apply = 
  print
  $ take 4 
  $ map (200 `div'`) [10, 20 ..]

test_pipe = 
  map (200 `div'`) [10, 20 ..] 
  & take 4 
  & print 

test_compose = 
  mapcat [1..4] & print
  where mapcat = (concat . (map (enumFromTo 1)))
  -- (enumFromTo 1) same as (\x->[1..x])
  -- concat same as (foldr (++) [])

test_id =
  id 1 & print

test_const = 
  ((const 1) "x") & print


data Person = Person {name :: String, age :: Int}
test_on = 
  let
    a = Person{ name="Foo", age=11 }
    b = Person{ name="Bar", age=22 }
  in
    sum_age a b & print
    where sum_age = (+) `on` (age)
