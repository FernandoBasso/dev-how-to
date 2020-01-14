--
-- Concrete types imply all the type classes they provide, p 206.
--

add :: Int -> Int -> Int
add x y = x + y

addWeird :: Int -> Int -> Int
addWeird x y =
  if x > 1
  then x + y
  else x

check' :: Int -> Int -> Bool
check' a a' = a == a'

--
-- A type like Int either has a type class constraint or it does not. Adding
-- (Ord a, Num a) does nothing. Int, thankfully, has both Ord and Num (and
-- others). A concrete type always implies the type classes provided for it.
--
