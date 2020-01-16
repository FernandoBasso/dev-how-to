--
-- Pattern Matching Tuples, p 236.
--

module TupleFunctions where

--
-- This tuple must be constitued of two values of the same type
-- because (+) requires it.
--
addEmUp :: Num a => (a, a) -> a
addEmUp (x, y) = (+) x y

-- Instead of
addEmUp' :: Num a => (a, a) -> a
addEmUp' tup = (+) (fst tup) (snd tup)

--
--    λ addEmUp (10, 20)
--    30
--    λ addEmUp' (10, 20)
--    30
--

--
-- Retrieves first, middle, and last element of a three-tuple.
--
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

mid3 :: (a, b, c) -> b
mid3 (_, y, _) = y

lst3 :: (a, b, c) -> c
lst3 (_, _, z) = z

--
--    λ> fst3 (10, 20, 30)
--    10
--    λ> mid3 (10, 20, 30)
--    20
--    λ> lst3 (10, 20, 30)
--    30
--

--
--    λ> :browse TupleFunctions
--    addEmUp :: Num a => (a, a) -> a
--    addEmUp' :: Num a => (a, a) -> a
--    fst3 :: (a, b, c) -> a
--    mid3 :: (a, b, c) -> b
--    lst3 :: (a, b, c) -> c
--

