--
-- Ord implies Eq, p 193.
-- We have no Eq instance for check'.
--
check' :: a -> a -> Bool
check' a a' = a == a'

--
--    λ> :load e16-check-broken.hs
--    [1 of 1] Compiling Main             ( e16-check-broken.hs, interpreted )
--
--    e16-check-broken.hs:5:15: error:
--        • No instance for (Eq a) arising from a use of ‘==’
--          Possible fix:
--            add (Eq a) to the context of
--              the type signature for:
--                check' :: forall a. a -> a -> Bool
--        • In the expression: a == a'
--          In an equation for ‘check'’: check' a a' = a == a'
--      |
--    5 | check' a a' = a == a'
--      |               ^^^^^^^
--    Failed, no modules loaded.
--
