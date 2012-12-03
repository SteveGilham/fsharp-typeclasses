module Control.Monad.Cont

open Prelude
open Control.Applicative

type Cont<'R,'A> = Cont of (('A->'R)->'R) with
    static member instance (Functor.Fmap,   Cont m:Cont<'r,'a>, _) = fun (f:_->'b) -> Cont(fun c -> m (c << f))

let runCont (Cont x) = x
type Cont<'R,'A> with
    static member instance (Monad.Return, _:Cont<'r,'a>          ) = fun n -> Cont(fun k -> k n)                         :Cont<'r,'a>
    static member instance (Monad.Bind  ,   Cont m, _:Cont<'r,'b>) = fun f -> Cont(fun k -> m (fun a -> runCont(f a) k)) :Cont<'r,'b>

type Cont<'R,'A> with
    static member instance (Applicative.Pure, _:Cont<'r,'a>) = fun (x:'a) -> Applicative.pure' x :Cont<'r,_>
    static member instance (Applicative.Ap, f:Cont<'r,_>, x:Cont<'r,'a>, _:Cont<'r,'b>) = fun () -> Applicative.ap f x :Cont<'r,'b>

let callCC (f:(_->Cont<'r,'b>)->_) = Cont <| fun k -> runCont (f (fun a -> Cont(fun _ -> k a))) k