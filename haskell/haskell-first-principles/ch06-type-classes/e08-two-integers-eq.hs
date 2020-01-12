
--
-- EXERCISES: Eq Instances p 180.
-- Exercise 2, p 180.
--

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two v1 v2) (Two v1' v2') =
    v1 == v1' && v2 == v2'

--
--    λ> :load dev.hs
--    λ> Two 1 2 == Two 1 (5 - 3)
--    True
--    λ> Two 1 2 == Two 1 3
--    False
--
