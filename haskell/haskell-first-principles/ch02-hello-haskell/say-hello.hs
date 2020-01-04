sayHello :: String -> IO ()
sayHello s =
  putStrLn ("Hello, " ++ s ++ "!")
