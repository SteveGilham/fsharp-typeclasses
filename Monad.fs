module Control.Monad

open Prelude

let inline liftM2 f m1 m2 = do' { let! x1 = m1 in let! x2 = m2 in return (f x1 x2) }
let inline ap x = liftM2 id x

