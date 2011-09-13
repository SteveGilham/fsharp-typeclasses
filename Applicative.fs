module Control.Applicative

open Prelude
open Control.Monad

let inline pure' x = return' x

type Apply = Apply with
    static member (?<-) (f:option<_> ,cs:Apply ,x:option<_> ) = ap f x
    static member (?<-) (f:list<_>   ,cs:Apply ,x:list<_>)    = ap f x
    static member (?<-) (f:IO<_>     ,cs:Apply ,x )           = ap f x
    static member (?<-) (f:_ -> _    ,cs:Apply ,g: _ -> _ )   = fun x -> (f x) (g x)

let inline (<*>) x y = x ? (Apply) <- y

let inline (<<|>) f a   = fmap f a
let inline liftA2 f a b = f <<|> a <*> b
let inline (  *>)   x   = x |> liftA2 (const' id)
let inline (<*  )   x   = x |> liftA2 const'
let inline (<**>)   x   = x |> liftA2 (|>)