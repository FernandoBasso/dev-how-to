module GreetIfCool1 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool
    then putStrLn "Hey! What's shakin'‽"
  else
    putStrLn "pshhhh..."
  where cool = coolness == "we cool"

{-
:load greet-if-cool-1.hs

greetIfCool "Hello"

greetIfCool "we cool"
-}
