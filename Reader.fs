module Control.Monad.Reader

open Prelude
type Reader<'r,'a> = Reader of ('r->'a) with
    static member (?<-) (_       , _Functor:Fmap  ,   Reader m   ) = fun f -> Reader(fun r -> f (m r))

let runReader (Reader x) = x
type Reader<'s,'a> with
    static member (?<-) (_       , _Monad  :Return, _:Reader<_,_>) = fun a -> Reader(fun _ -> a)
    static member (?<-) (Reader m, _Monad  :Bind  , _:Reader<_,_>) = fun k -> Reader(fun r -> runReader(k (m r)) r)


let mapReader  f (Reader m) = Reader(f << m)
let withReader f (Reader m) = Reader(m << f)
let ask                = Reader id
let local f (Reader m) = Reader(m << f)
