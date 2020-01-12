--
-- EXERCISES: Eq Instances p 180.
-- Exercise 6, p 181.
--

data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'
  (==) _ _ = False

--
-- Without the `_ _' pattern, we get a non-exhaustive pattern warning.
--
--     λ> x = ThisOne (5 :: Integer)
--     λ> y = ThisOne (5 :: Integer)
--     λ> x == y
--     True
--     λ> (/=) y (ThisOne (4 :: Integer))
--     True
--
