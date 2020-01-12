
--
-- EXERCISES: Eq Instances p 180.
-- Exercise 3, p 181.
--

data StringOrInt =
    TisAnInt    Int
  | TisAString  String


instance Eq StringOrInt where
  (==) (TisAnInt i) (TisAnInt i') = i == i'
  (==) (TisAString s) (TisAString s') = s == s'


--
--    λ> n = TisAnInt 5
--    λ> :type n
--    n :: StringOrInt
--    λ> s = TisAString "Dual Pistols"
--    λ> :type s
--    s :: StringOrInt
--
--
--    λ> TisAnInt 5 == TisAnInt 6
--    False
--    λ> TisAnInt 3 == TisAnInt (5 - 2)
--    True
