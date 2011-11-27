module Control.Applicative

open Prelude
open Control.Monad.Base

let inline pure' x = return' x

type Apply = Apply with
    static member (?<-) (f:option<_>, _Applicative:Apply, x:option<_>) = ap f x
    static member (?<-) (f:list<_>  , _Applicative:Apply, x:list<_>  ) = ap f x
    static member (?<-) (f:IO<_>    , _Applicative:Apply, x          ) = ap f x
    static member (?<-) (f:_ -> _   , _Applicative:Apply, g: _ -> _  ) = ap f g

let inline (<*>) x y = x ? (Apply) <- y


type Empty = Empty with
    static member (?<-) (_:unit, _Alternative:Empty, _:'a option) = None
    static member (?<-) (_:unit, _Alternative:Empty, _:'a list  ) = []

let inline empty() : ^R = (() ? (Empty) <- Unchecked.defaultof< ^R>)


type Append = Append with    
    static member (?<-) (x:option<_>, _Alternative:Append, y) = match x with | None -> y | xs -> xs
    static member (?<-) (x:list<_>  , _Alternative:Append, y) = List.append  x y
    
let inline (<|>) (x:'a) (y:'a) : 'a = x ? (Append) <- y



let inline (<<|>) f a   = fmap f a
let inline liftA2 f a b = f <<|> a <*> b
let inline (  *>)   x   = x |> liftA2 (const' id)
let inline (<*  )   x   = x |> liftA2 const'
let inline (<**>)   x   = x |> liftA2 (|>)

let inline optional v = Some <<|> v <|> pure' None

type ZipList<'a> = ZipList of 'a seq with
    static member (?<-) (_:unit   , _Functor    :Fmap  ,   ZipList x  ) = fun f -> ZipList (Seq.map f x)
    static member (?<-) (_:unit   , _Applicative:Return, _:ZipList<'a>) = fun x -> ZipList (Seq.initInfinite (fun _ -> x))
    static member (?<-) (ZipList f, _Applicative:Apply ,   ZipList x  ) = ZipList (Seq.zip f x |> Seq.map (fun (f,x) -> f x))