module Control.Applicative

open Prelude
open Control.Monad

let inline pure' x = return' x

type Apply() =   
    static member (?<-) (f:option<_> ,cs:Apply ,x:option<_> ) = ap f x
    static member (?<-) (f:list<_>   ,cs:Apply ,x:list<_>)    = ap f x
    static member (?<-) (f:IO<_>     ,cs:Apply ,x )           = ap f x
    static member (?<-) (f:_ -> _    ,cs:Apply ,g: _ -> _ )   = fun x -> (f x) (g x)

let inline (<*>) x y = x ? (Apply()) <- y


