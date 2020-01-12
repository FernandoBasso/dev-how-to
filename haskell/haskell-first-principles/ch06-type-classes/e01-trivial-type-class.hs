{-# LANGUAGE NoMonomorphismRestriction #-}

data Trivial =
  Trivial'

--
-- Implements Eq type class for the Trivial data type.
--
instance Eq Trivial where
  Trivial' == Trivial' = True

--
-- Used ' at the end of the data constructor to avoid confusion. Trivial'
-- is the data constructor and Trivial is the type constructor.
--
-- Trivial' is data, like 9, or "foo". You compare the data, not the type.

-- You do not do:
--
--    λ> Trivial == Trivial
--
-- Rather, you do:
--
--    λ> Trivial' == Trivial'
--    True
--
