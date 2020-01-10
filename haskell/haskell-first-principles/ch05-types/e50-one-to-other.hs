--
-- Given a type, write a function (page 153).
--

fn :: (x -> y)
   -> (y -> z)
   -> c
   -> (a, x)
   -> (a, z)
fn xToY yToZ _ (a, x) =
  (a, (yToZ (xToY x)))
