--
-- Parametricity exercise 2.
--
f1 :: a -> a -> a
f1 x y = x

f2 :: a -> a -> a
f2 x y = y

--
-- λ> f1 "Tomb" "Raider"
-- "Tomb"
-- λ> f2 "Tomb" "Raider"
-- "Raider"
--

--
-- Parametricity exercise 3.
--
h :: a -> b -> b
h x y = y

--
-- λ> h "Tomb Raider" "The Last Revelation"
-- "The Last Revelation"
--
