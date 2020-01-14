--
-- EXERCISES: Will They Work? p 194.
-- Exercise 1, p 194.
--

--
-- NOTE: We can't simply do `max ...' in the top level.
--
result :: Int
result = max (length [1, 2, 3])
             (length [8, 9, 10, 11, 12])

main :: IO ()
main = do
  print result

--
--    λ> :load dev.hs
--    λ> main
--    5
--
