--
-- EXERCISES: Type-Known-Do, p 156.
-- Exercise 3, p 158.
--

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform tup = (xz (fst tup), yz (snd tup))
