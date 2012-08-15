module Control.Monad.Cont

open Prelude
type Cont<'r,'a> = Cont of (('a->'r)->'r) with
    static member (?<-) (_Functor:Fmap  ,   Cont m, _) = fun f -> Cont(fun c -> m (c << f))

let runCont (Cont x) = x
type Cont<'r,'a> with
    static member (?<-) (_Monad  :Return, _:Cont<'r,'a>, _       ) = fun n -> Cont(fun k -> k n)                         :Cont<'r,'a>
    static member (?<-) (_Monad  :Bind  ,   Cont m, _:Cont<'r,'b>) = fun f -> Cont(fun k -> m (fun a -> runCont(f a) k)) :Cont<'r,'b>

let callCC f = Cont <| fun k -> runCont (f (fun a -> Cont(fun _ -> k a))) k