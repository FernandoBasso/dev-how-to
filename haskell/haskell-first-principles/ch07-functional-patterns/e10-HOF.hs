--
-- Higher-order functions, p 240.
--

--
-- Higher-order functions are functions that accept functions as
-- arguments.
--

--
--    λ> :t flip
--    flip :: (a -> b -> c) -> b -> a -> c
--    λ> (-) 10 1
--    9
--


--
-- p 241.`flip' takes a function of two parameters and flips the order
-- of those parameters.
--

fSub = flip (-)
--
--    λ> fSub 10 1
--    -9
--

myflip :: (a -> b -> c) -> b -> a -> c
myflip f x y = f y x

myflip' :: (a -> b -> c) -> b -> a -> c
myflip' f = \x y -> f y x
--
--    λ> f = myflip' (-)
--    λ> :t f
--    f :: Num c => c -> c -> c
--    λ> f 10 1
--    -9
--
--

--
-- p 242. Review this in the book. VERY important.
--

returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' _ _ _ d = d

--
-- This would be wrong:
--
--    returnLast :: (((a -> b) -> c) -> d) -> d
--
-- `->' is right-associative, although functions are applied from left to right.
--

--
-- p 243.
--

returnAfterApply :: (a -> b) -> a -> c -> b
returnAfterApply f a c = f a
--
-- c is ignored.
--
--    λ> returnAfterApply (\x -> (+) x 1) 10 20
--    11
--
