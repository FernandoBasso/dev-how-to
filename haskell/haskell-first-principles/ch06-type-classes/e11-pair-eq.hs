--
-- EXERCISES: Eq Instances p 180.
-- Exercise 5, p 181.
--

data Tuple a b =
  Tuple a b

-- @TODO: Why (Tuple a b) and in the previous exercise we have
-- only `(Pair a)' instead of `(Pair a a)'?
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a1 b1) (Tuple a2 b2) =
    a1 == a2 && b1 == b2

--
--    λ> :load dev.hs
--    λ> Tuple 'x' 'y' == Tuple 'x' 'y'
--    True
--    λ> Tuple 'x' 'y' /= Tuple 'x' 'y'
--    False
--    λ> Tuple 'x' 'y' /= Tuple 'x' 'k'
--    True
--
