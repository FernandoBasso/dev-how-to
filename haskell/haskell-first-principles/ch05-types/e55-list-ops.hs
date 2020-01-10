--
-- Given a type, write the function (p 154)
-- Exercise 5, p 155.
--
-- Involves operations on lists. It could get the tail, reverse, (may be
-- `sort' too), and possibly others.
--

--
-- Just return the list itself, like the `id' function.
--
r1 :: [a] -> [a]
r1 xs = xs

--
-- Reverse.
--
rev :: [a] -> [a]
rev (x:xs) = xs ++ [x]
--
-- λ> rev [1, 5, 9]
-- [5,9,1]
-- λ> rev "jedi"
-- "edij"
--

--
-- Tail. (all elements except the head/first).
--
mytail :: [a] -> [a]
mytail (_:xs) = xs
--
-- λ> mytail [1, 5, 9]
-- [5,9]
-- λ> mytail "X-Men"
-- "-Men"
--
