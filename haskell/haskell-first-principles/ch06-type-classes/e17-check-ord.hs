--
-- Ord implies Eq, p 193.
--

check' :: Ord a => a -> a -> Bool
check' a a' = a == a'

--
--    λ> :load e17-check-ord.hs
--    [1 of 1] Compiling Main             ( e17-check-ord.hs, interpreted )
--    Ok, one module loaded.
--    λ> check' 5 5
--    True
--    λ> check' 'z' 'z'
--    True
--    λ> check' [1, 2] [1, 3 - 1]
--    True
--
-- Anything that provides an instance of Ord must by definition also already
-- have an instance of Eq. How do we know? As we said above, logically it
-- makes sense that you can’t order things without the ability to check for
-- equality, but we can also check :info Ord in GHCi:
--
--    λ> :info Ord
--    class Eq a => Ord a where
--    ...more output elided...
--
-- The class definition of Ord says that any a which wants to define an Ord
-- instance must already provide an Eq instance. We can say that Eq is a
-- superclass of Ord.
--
-- We should just have used Eq here, since we don't really need Ord. But it
-- was worth doing it with Ord to learn about the fact that Ord implies Eq
-- because Eq is like a superclass of Ord.
--
