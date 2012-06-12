module Control.Monad.Cont

open Prelude
type Cont<'r,'a> = Cont of (('a->'r)->'r) with
    static member fmap (Functor, Cont m      ) = fun f -> Cont(fun c -> m (c << f))

let runCont (Cont x) = x
type Cont<'r,'a> with
    static member return' (Monad, _:Cont<'r,'a>) = fun n -> Cont(fun k -> k n)                         :Cont<'r,'a>
    static member bind (Monad,Cont m  , _:Cont<'r,'b>) = fun f -> Cont(fun k -> m (fun a -> runCont(f a) k)) :Cont<'r,'b>

let callCC f = Cont <| fun k -> runCont (f (fun a -> Cont(fun _ -> k a))) k