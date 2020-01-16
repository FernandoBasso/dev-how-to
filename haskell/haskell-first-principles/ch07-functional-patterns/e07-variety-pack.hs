--
-- EXERCISES, Variety Pack, p 237.
--

--------------------------------------------------------------------------------
-- #1, p 237.
--

k (x, y) = x

k1 = k ((4 - 1), 10)

k2 = k ("three", (1 + 2))

k3 = k (3, True)


--
-- #1a
--
--    λ> :t k
--    k :: (a, b) -> a
--

--
-- #1b
--
--    λ> :t k1
--    k1 :: Integer
--
-- We fully applied k, which returns x, the first element of the tuple.
--

--
-- #1c
--
-- k1 and k3 both return 3 as a result.
--

--------------------------------------------------------------------------------
-- #2, p 237.
--

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))
--
--    λ> f ('a', 'b', 'c') ('d', 'e', 'f')
--    (('a','d'),('c','f'))
--
--    λ> f ("Tomb", "Raider", "VI") ("Angel", "Of", "Darkness")
--    (("Tomb","Angel"),("VI","Darkness"))
--

