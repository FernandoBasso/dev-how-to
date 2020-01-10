--
-- EXERCISES: Type-Known-Do, p 156.
-- Exemple for the next exercises, p 157.
--

data Woot
data Blah

f :: Woot -> Blah
f = undefined

g :: (Blah, Woot) -> (Blah, Blah)
g :: (b, w) = (b, f w)

--
-- Because `f' takes a `Woot' and produces a `Blah', we can apply `f' to `w'
-- and the typechecker is happy!
--
