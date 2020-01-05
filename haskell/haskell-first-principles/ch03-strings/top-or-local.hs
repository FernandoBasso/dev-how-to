-- top-or-local.hs
module ToOrLocal where

topLevelFn :: Integer -> Integer
topLevelFn x =
  x + woot + topLevelValue
  where woot :: Integer
        woot = 10

topLevelValue :: Integer
topLevelValue = 5
