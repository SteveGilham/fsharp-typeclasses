module Data.Traversable

open Prelude
open Control.Applicative
open Data.Foldable

type Traverse = Traverse with
    static member inline (?<-) (f, _Traversable:Traverse, t:Maybe<_>) =
        match t with
        | Nothing -> pure' Nothing
        | Just x  -> fmap  Just (f x)
     
    static member inline (?<-) (f, _Traversable:Traverse, t:List<_> ) =
        let cons x y = x :: y
        let cons_f x ys = fmap cons (f x) <*> ys
        (foldr cons_f (pure' [] )) t

let inline traverse f t = f ? (Traverse) <- t
let inline sequenceA  x = traverse id x