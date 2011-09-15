﻿module Data.Traversable

open Prelude
open Control.Applicative
open Data.Foldable

type Traverse = Traverse with
    static member inline (?<-) (fn ,cs:Traverse,t)  =
        match t with
        | None   -> pure' None
        | Some x -> (fmap (fun o -> Some o) (fn x))
     
    static member inline (?<-) (f,cs:Traverse,t:list<_>) =
        let cons x y = x :: y
        let cons_f x ys = fmap cons (f x) <*> ys
        (foldr cons_f (pure' [] )) t

let inline traverse fn t = fn ? (Traverse) <- t

let inline sequenceA x = traverse id x
