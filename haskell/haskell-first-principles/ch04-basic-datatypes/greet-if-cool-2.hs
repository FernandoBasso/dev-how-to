module GreetIfCool2 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool coolness
    then putStrLn "Eyyy. What's shakin'"
  else
    putStrLn "Pshhh.."
  where cool v = v == "downright frosty yo"
