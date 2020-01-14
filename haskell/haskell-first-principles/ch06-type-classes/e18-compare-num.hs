--
-- EXERCISES: Will They Work? p 194.
-- Exercise 2, p 194.
--

result :: Ordering
result = compare (3 * 4) (3 * 5)

main :: IO ()
main = do
  print result


--
--    λ> :load dev.hs
--    λ> main
--    LT
--
