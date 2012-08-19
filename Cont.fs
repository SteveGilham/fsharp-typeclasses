module Control.Monad.Cont

open Prelude
type Cont<'R,'A> = Cont of (('A->'R)->'R) with
    static member instance (_Functor:Fmap,   Cont m:Cont<'r,'a>, _) = fun (f:_->'b) -> Cont(fun c -> m (c << f))

let runCont (Cont x) = x
type Cont<'R,'A> with
    static member instance (_Monad:Return, _:Cont<'r,'a>          ) = fun n -> Cont(fun k -> k n)                         :Cont<'r,'a>
    static member instance (_Monad:Bind  ,   Cont m, _:Cont<'r,'b>) = fun f -> Cont(fun k -> m (fun a -> runCont(f a) k)) :Cont<'r,'b>

let callCC (f:(_->Cont<'r,'b>)->_) = Cont <| fun k -> runCont (f (fun a -> Cont(fun _ -> k a))) k