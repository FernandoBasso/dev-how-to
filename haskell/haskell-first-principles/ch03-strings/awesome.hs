
exclaim :: [Char] -> [Char]
exclaim s = (++) s "!"

fifthChar :: [Char] -> Char
fifthChar s = (!!) s 4

from9thOn :: [Char] -> [Char]
from9thOn s = drop 12 s

thirdChar :: [Char] -> Char
thirdChar s = (!!) s 2

charIndex :: Int -> Char
charIndex i = (!!) "Currying is awesome!" i


main :: IO ()
main = do
  putStrLn (exclaim "Curry is awesome")
  -- Use [ ... ] to make a string out of the char
  -- so we can putStrLn it.
  putStrLn [(fifthChar "Currying is awesome!")]
  putStrLn (from9thOn "Currying is awesome!")
  putStrLn [(thirdChar "Currying is awesome!")]
  putStrLn [charIndex 3]
