--
-- EXERCISES, Case Practice, p 239.
--

--------------------------------------------------------------------------------
-- #1, p 240
--

fnC :: (Ord a, Num a) => a -> a -> a
fnC x y = if (x > y) then x else y

fnC' :: (Ord a, Num a) => a -> a -> a
fnC' x y =
  case result of
    True  -> x
    False -> y
  where result = x > y
--
--    λ> fnC' 4 1
--    4
--    λ> fnC' 3 5
--    5
--

--------------------------------------------------------------------------------
-- #2, p 240
--

ifEvenAdd2 :: Integer -> Integer
ifEvenAdd2 n = if even n then (+) n 2 else n

ifEvenAdd2' :: Integer -> Integer
ifEvenAdd2' n =
  case testResult of
    True  -> (+) n 2
    False -> n
  where testResult = even n
--
--    λ> ifEvenAdd2' 8
--    10
--    λ> ifEvenAdd2' 9
--    9
--

--
-- Tried this myself to use case/of with let/in instead of where.
--

incrIfEven :: Integer -> Integer
incrIfEven n =
  let
    testResult = even n
  in
    case testResult of
      True  -> (+) n 1
      False -> n
--
--    λ> incrIfEven 2
--    3
--    λ> incrIfEven 5
--    5
--

--------------------------------------------------------------------------------
-- #3, p 240.
--

nums :: (Ord a, Num a) => a -> a
nums n =
  case compare n 0 of
    LT -> -1
    GT ->  1
    EQ ->  0
--
--    λ> nums 4
--    1
--    λ> nums (negate 3)
--    -1
--    λ> nums 0
--    0
--

