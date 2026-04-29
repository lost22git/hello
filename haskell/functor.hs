-- typeclass Functor
--
-- class Functor f where
--   fmap :: (a->b) -> f a ->  f b
-- 

import Text.Printf

main = do
  test_function
  test_list

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
