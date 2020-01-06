module Reverse where

-- Reverse "Curry is awesome" to
-- "awesome is Curry"

rev :: [Char] -> [Char]
rev s = wEnd ++ " " ++ wMid ++ " " ++ wIni
  where
    wIni = take 5 s
    wMid = take 2 (drop 6 s)
    wEnd = drop 9 s

main :: IO ()
main = do
  putStrLn $ rev "Curry is awesome"

-- curry is awesome
