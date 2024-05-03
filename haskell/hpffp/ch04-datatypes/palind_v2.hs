----
-- Checks whether its input list is a palindrome.
--
-- Even though it uses non-safe functions, checks for lists of zero
-- or one element, so it is defined for all lists and is not a
-- partial function.
--
isPalind :: (Eq a) => [a] -> Bool
isPalind []       = True
isPalind (_ : []) = True
isPalind xs       =
  if (head xs) /= (last xs)
  then False
  else isPalind (init (tail xs))
