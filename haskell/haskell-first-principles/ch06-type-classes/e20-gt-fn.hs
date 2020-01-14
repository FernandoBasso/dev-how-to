--
-- EXERCISES: Will They Work? p 194.
-- Exercise 4, p 194.
--

result :: Bool
result = (5 + 3) > (3 + 6)

main :: IO ()
main = do
  print result

--
--    λ> :load dev.hs
--    λ> main
--    False
--
