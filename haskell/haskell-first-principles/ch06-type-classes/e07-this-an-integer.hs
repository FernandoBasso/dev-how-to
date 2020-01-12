--
-- EXERCISES: Eq Instances p 180.
-- Exercise 1, p 180.
--

{-# LANGUAGE NoMonomorphismRestriction #-}

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn v) (TisAn v') = v == v'

--
--     λ :load dev.hs
--     λ x = TisAn 3
--     λ y = TisAn 3
--     λ :type y
--     y :: TisAnInteger
--     λ x == y
--     True
--     λ x /= y
--     False
--
