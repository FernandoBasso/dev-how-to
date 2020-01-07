
--
-- This is supposed to be the identity function, id.
--

id' = \x -> x

main :: IO ()
main = do
  putStrLn $ id' "IT FUCKING WORKS!"
  putStrLn $ show $ id' 13
  putStrLn (show (id' 13))
