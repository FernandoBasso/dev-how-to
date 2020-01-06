module Palindrome where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome s = s == reverse s

main :: IO ()
main = do
  print (isPalindrome "ana")
  print (isPalindrome "racecar")
  print (isPalindrome "banana")
