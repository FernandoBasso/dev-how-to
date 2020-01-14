--
-- Gimme more operations, p 204.
--

--
-- No constraint on term-level values mean they could be any type,
-- therefore, there is not much we can do with them. How can you add
-- or multiply values you don't know are whether numbers or not‽ The
-- methods and operations are in the type classes, and so we get more
-- utility by specifying type class constraints. If your types are
-- more general than your terms are, then you need to constrain your
-- types with the type classes that provide the operations you want to
-- use.


--
-- We need a type class constraint otherwise GHC has no way to know
-- how to use the (+) function with x and y. (+) comes from Num. GHC
-- won't allow us to attempt to use (+) unless we add a constraint
-- with a type class that has (+).
--

--
-- Broken. GHC can't use (+) with an uknown type.
--
--    add :: a -> a -> a
--    add x y = x + y
--

--
-- Broken, we need Ord to use >.
--
--    addWeird :: Num a => a -> a -> a
--    addWeird x y =
--      if x > 1
--      then x + y
--      else x
--

addWeird :: (Ord a, Num a) => a -> a -> a
addWeird x y =
  if x > 1
  then x + y
  else x

--
--    λ> addWeird 1
--    addWeird 1 :: (Ord a, Num a) => a -> a
--    λ> addWeird 1 3
--    1
--    λ> addWeird 2 3
--    5
--

