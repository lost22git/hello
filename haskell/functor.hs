-- Functor
--
-- class Functor f where
--   fmap :: (a->b) -> f a ->  f b
-- 

import Text.Printf

main = do
  test_function
  test_list
  test_applicative
  test_sequenceA

-- Function as a instance of Functor
-- same as compose
--
-- instance Functor ((->) r) where
--   fmap f g = \x -> f (g x) 
--   -- fmap = (.)
--
test_function = do
  printf "compose functions via .: %d\n" $ (*2) . (*3) $ 7
  printf "compose functions via functor fmap: %d\n" $ fmap (*2) (*3) $ 7
  printf "compose functions via functor fmap <$>: %d\n" $ (*2) <$> (*3) $ 7


-- List as a instance of Functor
-- same as map
--
-- instance Functor List where
--   fmap = map
--
test_list = do
  print $ (*2) <$> [1,3..10]

-- Applicative Functor
--
-- class (Functor f) => Applicative f where
--     pure :: a -> f a
--     (<*>) :: f (a -> b) -> f a -> f b
--
-- List as a instance of Applicative
--
-- instance Applicative [] where  
--     pure x = [x]  
--     fs <*> xs = [f x | f <- fs, x <- xs]
--
test_applicative = do
  putStrLn $ "=== applicative ==="
  print $ (+) <$> [1, 2] <*> [3, 4] 
  -- liftA2 f a b = f <$> a <*> b
  print $ liftA2 (+) [1, 2] [3, 4] 
  print $ [(a + b) | a <- [1, 2], b <- [3, 4]]

test_sequenceA = do
  putStrLn $ "=== sequenceA ===" 
  -- sequenceA [f a] = f [a]
  print $ sequenceA [[1, 2], [3, 4]]
  -- \x -> [(x+2), (x*2)]
  print $ sequenceA [(+2), (*2)] 5 
  
