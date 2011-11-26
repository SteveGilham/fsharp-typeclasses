module Data.Traversable

open Prelude
open Control.Applicative
open Data.Foldable

type Traverse = Traverse with
    static member inline (?<-) (f, _Traversable:Traverse, t)  =
        match t with
        | None   -> pure' None
        | Some x -> fmap Some (f x)
     
    static member inline (?<-) (f, _Traversable:Traverse, t:list<_>) =
        let cons x y = x :: y
        let cons_f x ys = fmap cons (f x) <*> ys
        (foldr cons_f (pure' [] )) t

let inline traverse f t = f ? (Traverse) <- t
let inline sequenceA x = traverse id x