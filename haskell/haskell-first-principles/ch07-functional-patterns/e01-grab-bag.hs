--
-- EXERCISES: Grab Bag, p 226.
--
-- https://github.com/mvaldesdeleon/haskell-book/blob/master/ch07/exercises.md
--


--------------------------------------------------------------------------------
-- #1, p 226
--

mTh1 x y z = x * y * z

mTh2 x y = \z -> x * y * z

mTh3 x = \y -> \z -> x * y * z

mTh4 = \x -> \y -> \z -> x * y * z


--
-- Looks like they are all equivalent:
--
--     λ> mTh1 2 5 2
--     20
--     λ> mTh2 2 5 2
--     20
--     λ> mTh3 2 5 2
--     20
--     λ> mTh4 2 5 2
--     20
--
--

--------------------------------------------------------------------------------
-- #2, p 226.
--

--
-- Letter D is correct. The type of `mTh1 3' is
--
--    Num a => a -> a -> a
--


--------------------------------------------------------------------------------
-- #3, p 227.
--

--
--    addOne x = x + 1
--
-- Can be written as:
--
--    addOne = \x -> x + 1
--

--
-- #3a, p 227.
--

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f n = n + 1


addOneIfOdd' n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

--
-- NOTE that f does not take n. Rather, f is the lambda expression
-- that takes n.
--

--
-- #3b, p 227.
--

addFive x y = (if x > y then y else x) + 5

addFive' = \x -> \y -> (if x > y then y else x) + 5


--
-- #3c, p 227.
--

mflip f = \x -> \y -> f y x

mflip' f x y = f y x

