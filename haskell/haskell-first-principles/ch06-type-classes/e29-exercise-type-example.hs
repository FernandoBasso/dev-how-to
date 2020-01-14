--
-- Chapter Exercises - Does it typecheck? p 208.
-- Instruction example, p 208.
--

x     :: Int -> Int
x blah = blah + 20

printIt :: IO ()
printIt = putStrLn (show x)

--
-- Functions cannot have a Show instance. Nothing with the function type
-- constructor (->) has an instance of show.
--
-- https://wiki.haskell.org/Show_instance_for_functions
--

{-
dev.hs:10:21: error:
    • No instance for (Show (Int -> Int)) arising from a use of ‘show’
        (maybe you haven't applied a function to enough arguments?)
    • In the first argument of ‘putStrLn’, namely ‘(show x)’
      In the expression: putStrLn (show x)
      In an equation for ‘printIt’: printIt = putStrLn (show x)
   |
10 | printIt = putStrLn (show x)
   |                     ^^^^^^
-}
