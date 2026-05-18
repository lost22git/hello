main = do
  test_function
  test_applicative
  test_sequenceA

-- Functor
--
-- class Functor f where
--   fmap :: (a->b) -> f a ->  f b
-- 
test_function = do
  putStrLn "=== Functor ==="

  -- instance Functor [a] where
  --   fmap :: (a->b) -> [a] -> [b]
  --   fmap g [] = []
  --   fmap g (x:xs) = (g x) : (fmap g xs)
  --   
  print $ fmap (*2) [1..10]
  print $ (*2) <$> [1..10] -- <$>: operator of fmap

  -- instance Functor (Maybe a) where
  --   fmap :: (a->b) -> Maybe a -> Maybe b
  --   fmap g (Just a) = Just (g a)
  --   fmap g Nothing = Nothing
  --
  print $ fmap (*2) (Just 1)
  print $ fmap (*2) Nothing

  -- instance Functor (->r) where
  --   fmap :: (a->b) -> (->a) -> (->b)
  --   fmap g k = g . k
  --
  print $ (fmap (*6) (+3)) 4
  

-- Applicative Functor
--
-- class (Functor f) => Applicative f where
--     pure :: a -> f a
--     (<*>) :: f (a -> b) -> f a -> f b
--
test_applicative = do
  putStrLn "=== Applicative ==="
  -- fmap2 :: (a->b->c) -> f a -> f b -> f c, how about?
  -- we have fmap :: (a->b->c) -> f a -> f (b->c)
  -- then how to f (b->c) -> f b -> f c
  -- That is Applicative
  --
  -- fmap0  :: a -> f a = pure a
  -- fmap   :: (a->b) -> f a -> f b
  -- fmap2  :: (a->b->c) -> f a -> f b -> f c = (fmap (a->b->c) (f a)) <*> (f b)
  -- fmap3  :: (a->b->c->d) -> f a -> f b -> f c -> f d = (fmap (a->b->c->d) (f a)) <*> (f b) <*> (f c)
  -- ...
  -- fmapN in stdlib is liftAN, e.g. fmap2 = liftA2
  

  -- instance Applicative (Maybe a) where
  --   pure a = (Just a)
  --   <*> (Maybe g) Nothing = Nothing
  --   <*> (Just g) (Just a) = Just (g a)
  --
  print $ (*) <$> Just 2 <*> pure 10
  print $ liftA2 (*) (Just 2) (pure 10)

  -- instance Applicative [a] where
  --   pure a = a : []
  --   <*> fs xs = [(f x) | f<-fs, x<-xs]
  --
  print $ (*) <$> [1, 2] <*> [3, 4]

test_sequenceA = do
  putStrLn "=== sequenceA ==="
  -- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
  print $ sequenceA [[1, 2], [3, 4]]
  -- \x -> [(x+2), (x*2)]
  print $ sequenceA [(+2), (*2)] 5 
