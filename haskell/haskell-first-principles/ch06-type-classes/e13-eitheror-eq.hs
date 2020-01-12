--
-- EXERCISES: Eq Instances p 180.
-- Exercise 7, p 181.
--

data EitherOr a b =
    Hello   a
  | Goodbye b
  deriving Show

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _ = False

--
-- @TODO: Figure out why with `:set -Wall' the REPL below produces
-- warnings about default types.
--
--    λ s1 = Hello ("lara" :: [Char])
--    λ s1
--    Hello "lara"
--    λ s1 == s1
--    True
--    λ Goodbye "croft" /= Goodbye "craft"
--    True
--
