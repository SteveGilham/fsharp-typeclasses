module Data.Traversable

open Prelude
open Control.Applicative
open Data.Foldable

type Traversable = Traversable with
    static member inline traverse (Traversable, f, t:Maybe<_>) =
        match t with
        | Nothing -> pure' Nothing
        | Just x  -> fmap Just (f x)
     
    static member inline traverse (Traversable, f, t:List<_> ) =
        let cons x y = x :: y
        let cons_f x ys = fmap cons (f x) <*> ys
        (foldr cons_f (pure' [] )) t

#nowarn "64"

let inline traverse f t = ((^C or ^a or ^b) : (static member traverse: ^C * ^a * ^b -> _) (Traversable, f, t))
let inline sequenceA  x = traverse id x