----
-- Produces a new list without the first and last elements.
--
-- Because this function is implemented in terms of init and
-- tail (which are not safe), it assumes the input list contains
-- at two or more elements.
--
torso :: [a] -> [a]
torso = init . tail

----
-- Checks whether the first and last element of a list are the
-- same value.
--
-- Because this function is implemented in terms of head and
-- tail (which are not safe), it assumes the input list contains
-- at least one element.
--
eqlEdges :: (Eq a) => [a] -> Bool
eqlEdges xs = (==) (head xs) (last xs)

----
-- Checks whether its input list is a palindrome.
--
-- Even though it uses two non-safe helper functions, because we
-- pattern-match and check for the input, this function is defined
-- for empyt lists, lists of one element, and lists with two or
-- more elements.
--
isPalind :: (Eq a) => [a] -> Bool
isPalind []        = True
isPalind (_ : [])  = True
isPalind xs
  | not $ eqlEdges xs = False
  | otherwise = isPalind $ torso xs
