module Print3Broken where

{-
printSecond :: IO ()
printSecond = do
  putStrLn greeting

main :: IO ()
main = do
  putStrLn greeting
  printSecond
  where greeting = "Doesn't work..."
-}

--
-- FIXED VERSION
--
printSecond :: [Char] -> IO ()
printSecond s = do
  putStrLn s

main :: IO ()
main = do
  putStrLn greeting
  printSecond greeting
  where greeting = "It works now!"
