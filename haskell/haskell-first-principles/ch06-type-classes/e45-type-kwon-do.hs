--
-- EXERCISES: Type-Kwon-Do Two: Electric Typealoo, p 212.
--

--
-- Exercise 1, p 213.
--
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB x y = (aToB x) == y


--
-- Exercise 2, p 213.
--
arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB i a = (+) (fromInteger i) (aToB a)

