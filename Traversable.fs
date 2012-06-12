module Data.Traversable

open Prelude
open Control.Applicative
open Data.Foldable

type Traversable = Traversable with
    static member inline traverse (Traversable, f, t)  =
        match t with
        | None   -> pure' None
        | Some x -> fmap Some (f x)
     
    static member inline traverse (Traversable, f, t:list<_>) =
        let cons x y = x :: y
        let cons_f x ys = fmap cons (f x) <*> ys
        (foldr cons_f (pure' [] )) t

#nowarn "64"

let inline traverse f t = ((^C or ^a or ^b) : (static member traverse: ^C * ^a * ^b -> _) (Traversable, f, t))
let inline sequenceA  x = traverse id x