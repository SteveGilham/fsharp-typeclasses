module Control.Monad.Cont

open Prelude
type Cont<'r,'a> = Cont of (('a->'r)->'r) with
    static member ( ? ) ( Cont m , _Functor:Fmap)                 = fun f -> Cont <| fun c -> m (c << f)

    static member (?<-) (_:Return, _Monad  :Return, t:Cont<_,'a>) = fun (n:'a) -> Cont (fun k -> k n)
    static member (?<-) (Cont m  , _Monad  :Bind, t:Cont<_,_>)                 =
        let runCont (Cont x) = x
        fun f -> Cont (fun k -> m (fun a -> runCont (f a) k))

let runCont (Cont x) = x

open Control.Monad.Trans
open Control.Monad.IO

type ContT< ^rma> = ContT of ^rma with
    static member inline ( ? ) (ContT m , _Functor:Fmap              ) = fun f -> ContT <| fun c -> m (c << f)

    static member inline (?<-) (_:Return, _Monad  :Return, t:ContT<_>) = fun a -> ContT ((|>) a)
    static member inline (?<-) (ContT m , _Monad  :Bind  , t:ContT<_>     ) =
        let inline runContT (ContT x) = x
        fun k -> ContT <| fun c -> m (fun a -> runContT (k a) c)

    static member inline (?<-) (m      , _MonadTrans:Lift, t:ContT<_>) = ContT ((>>=) m)

    static member inline (?<-) (x:IO<_>, _MonadTrans:LiftIO, t:ContT<_>) = lift x

let mapContT      f (ContT m) = ContT (f << m)
let withContT     f (ContT m) = ContT (m << f)
let inline runContT (ContT x) = x

open Control.Monad.Trans.Maybe
open Control.Monad.Trans.List

let _callCC f = Cont <| fun k -> runCont (f (fun a -> Cont <| fun _ -> k a)) k

type CallCC = CallCC with
    static member inline (?<-) (f, _MonadCont:CallCC, t:MaybeT<_>) = MaybeT(_callCC <| fun c -> runMaybeT(f (MaybeT << c << Some)))    
    static member inline (?<-) (f, _MonadCont:CallCC, t:ListT<_> ) = ListT (_callCC <| fun c -> runListT (f (ListT << c << singleton)))
    static member (?<-) (f, _MonadCont:CallCC, t:Cont<_,_>) = _callCC f
    static member inline (?<-) (f, _MonadCont:CallCC, t:ContT<_>) = ContT <| fun k -> runContT (f (fun a -> ContT <| fun _ -> k a)) k

let inline callCC f : ^R = (f ? (CallCC) <- Unchecked.defaultof< ^R>)