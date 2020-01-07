
nonsense :: Bool -> Integer
nonsense True = 10
nonsense False = 20

curriedFn :: Integer -> Bool -> Integer
curriedFn i b = i + (nonsense b)

uncurriedFn :: (Integer, Bool) -> Integer
uncurriedFn (i, b) = i + (nonsense b)

anon :: Integer -> Bool -> Integer
anon = \i b -> i + (nonsense b)

anonNested :: Integer -> Bool -> Integer
anonNested = \i -> \b -> i + (nonsense b)
