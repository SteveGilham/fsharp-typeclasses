module Control.Monad.Cont

open Prelude
type Cont<'r,'a> = Cont of (('a->'r)->'r) with
    static member (?<-) (_:Return, cs:Return, t:Cont<_,'a>) = fun (n:'a) -> Cont (fun k -> k n)
    static member (?) (m:Cont<_,_>, cs:Bind) =
        let runCont (Cont(x)) = x
        fun f -> Cont (fun k -> runCont m (fun a -> runCont (f a) k))

let runCont (Cont(x)) = x

let callCC f = Cont <| fun k -> runCont (f (fun a -> Cont <| fun _ -> k a)) k

