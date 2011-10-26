module Control.Applicative

open Prelude
open Control.Monad

let inline pure' x = return' x

type Apply = Apply with
    static member (?<-) (f:option<_>, _Applicative:Apply, x:option<_>) = ap f x
    static member (?<-) (f:list<_>  , _Applicative:Apply, x:list<_>  ) = ap f x
    static member (?<-) (f:IO<_>    , _Applicative:Apply, x          ) = ap f x
    static member (?<-) (f:_ -> _   , _Applicative:Apply, g: _ -> _  ) = ap f g

let inline (<*>) x y = x ? (Apply) <- y

let inline (<<|>) f a   = fmap f a
let inline liftA2 f a b = f <<|> a <*> b
let inline (  *>)   x   = x |> liftA2 (const' id)
let inline (<*  )   x   = x |> liftA2 const'
let inline (<**>)   x   = x |> liftA2 (|>)

type ZipList<'a> = ZipList of 'a seq with
    static member ( ? ) (ZipList x, _Functor: Fmap)                     = fun f -> ZipList (Seq.map f x)
    static member (?<-) (_:Return , _Applicative:Return, t:ZipList<'a>) = fun x -> ZipList (Seq.initInfinite (fun _ -> x))
    static member (?<-) (ZipList f, _Applicative:Apply , ZipList x)     = ZipList (Seq.zip f x |> Seq.map (fun (f,x) -> f x))