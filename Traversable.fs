module Data.Traversable

open Prelude
open Control.Applicative
open Data.Foldable

module Traversable = 
    type Traverse = Traverse with
        static member inline instance (Traverse, t:option<_>, _) = fun f ->
            match t with
            | None    -> pure' None
            | Some x  -> fmap  Some (f x)
    
        static member inline instance (Traverse, t:List<_>  , _) = fun f ->
            let cons x y = x :: y
            let cons_f x ys = fmap cons (f x) <*> ys
            (foldr cons_f (pure' [] )) t

let inline traverse f t = Inline.instance (Traversable.Traverse, t) f
let inline sequenceA  x = traverse id x