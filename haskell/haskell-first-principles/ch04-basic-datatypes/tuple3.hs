--
-- When fixed, this function will return 1 from the value (1, 2).
-- f (a b) = A
--

fst' (a, _) = a
snd' (_, b) = b

main :: IO ()
main = do
  putStrLn $ fst' ("Lara", "Croft")
  putStrLn $ snd' ("Lara", "Croft")
