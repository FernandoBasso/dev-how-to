--
-- Given a datatype declaration, what can we do? p 2010.
--

data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

--
-- 1. NOK
-- phew = Papu "chases" True
--
-- Papu takes Rocks and Yeah, not the string and the boolean directlly. It is
-- required that we use the data constructors. Fix:
--
phew' = Papu (Rocks "chases") (Yeah True)

--
-- 2. OK. Properly uses the data contructors.
--
truth = Papu (Rocks "chomskydoz") (Yeah True)

--
-- 3.
--
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'
--
--    λ> p1 = Papu (Rocks "foo") (Yeah False)
--    λ> p1
--    Papu (Rocks "foo") (Yeah False)
--    λ> equalityForall p1 p1
--    True
--    λ> equalityForall p1 (Papu (Rocks "foo") (Yeah (not False)))
--    False
--
