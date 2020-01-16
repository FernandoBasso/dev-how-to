--
-- Pattern Matching Tuples, p 235.
--

--
-- f :: (a, b) -> (c, d) -> ((b, d), (a, c))
-- f = x y = ((snd x, snd y), (fst x, fst y))
--

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

--
--    λ f ("master", "yoda") ("lara", "croft")
--    (("yoda","croft"),("master","lara"))
--
