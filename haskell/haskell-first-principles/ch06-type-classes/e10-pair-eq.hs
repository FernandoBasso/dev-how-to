--
-- EXERCISES: Eq Instances p 180.
-- Exercise 4, p 181.
--

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair v1 v2) (Pair v1' v2') =
    v1 == v1' && v2 == v2'

--
--    λ> :load dev.hs
--
--    λ> p1 = Pair (1 :: Int) (2 :: Int)
--    λ> :type p1
--    p1 :: Pair Int
--    λ> p2 = Pair (1 :: Int) (3 :: Int)
--    λ> p1 == p2
--    False
--    λ> p1 == p1
--    True
--    λ> p2 == Pair (1 :: Int) (3 :: Int)
--    True
--
