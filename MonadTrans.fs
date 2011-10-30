module Control.Monad.Trans

open Prelude
open Control.Applicative

let singleton x = [x]
let concat (x:'a list list) :'a list = List.concat x

module MaybeT =

    type MaybeT< ^ma > = MaybeT of (^ma ) with

        static member inline ( ? ) (MaybeT x, _Functor:Fmap) = fun f -> MaybeT (fmap (Option.map f) x)

        static member inline (?<-) (_:Return, _Monad:Return, t:MaybeT<_>) = MaybeT << return' << Some
        static member inline (?<-) (MaybeT x, _Monad:Bind, t:MaybeT< ^b>) =
            let runMaybeT (MaybeT m) = m
            fun (f: ^a -> MaybeT< ^b>) ->
                MaybeT <| do' {
                    let! maybe_value = x
                    return! match maybe_value with
                            | None       -> return' None
                            | Some value -> runMaybeT <| f value}

        static member inline (?<-) (f:MaybeT<_->_> ,_Applicative:Apply ,t:MaybeT< ^b> ) = fun x -> ap f x

        static member inline (?<-) (_       , _MonadPlus:Mzero, t:MaybeT<_>)   = MaybeT (return' None)
        static member inline (?<-) (MaybeT x, _MonadPlus:Mplus,MaybeT y    ) =
            MaybeT <| do' {
                let! maybe_value = x
                return! match maybe_value with
                        | None -> y
                        | Some value -> x}

    let inline mapMaybeT f (MaybeT m) = MaybeT (f m)
    let inline runMaybeT   (MaybeT m) = m


module ListT =

    type ListT< ^ma > = ListT of (^ma ) with

        static member inline ( ? ) (ListT x     , _Functor:Fmap) = fun f -> ListT (fmap (List.map f) x)

        static member inline (?<-) (_:Return    , _Monad:Return, t:ListT<_>) = ListT << return' << singleton
        static member inline (?<-) (ListT x, _Monad:Bind, t:ListT< ^b>) =
            let inline runListT (ListT m) = m
            fun (k: ^a -> ^b ListT) -> ListT ( x >>= mapM (  (runListT) << k)  >>= (concat >> return') )

        static member inline (?<-) (f:ListT<_->_>, _Applicative:Apply, t:ListT< ^b> ) = fun x -> ap f x

        static member inline (?<-) (_      , _MonadPlus:Mzero, t:ListT<_>) = ListT (return' [])
        static member inline (?<-) (ListT x, _MonadPlus:Mplus, ListT y   ) = ListT <| do' {
            let! a = x
            let! b = y
            return (a @ b)}

    let inline mapListT f (ListT  m) = ListT (f m)
    let inline runListT   (ListT  m) = m

open MaybeT
open ListT

type Lift = Lift with
    static member inline (?<-) (x, _MonadTrans:Lift, t:MaybeT<_>) = MaybeT << (liftM Some)      <| x
    static member inline (?<-) (x, _MonadTrans:Lift, t: ListT<_>) = ListT  << (liftM singleton) <| x

let inline lift x : ^R = (x ? (Lift) <- Unchecked.defaultof< ^R>)


type LiftIO = LiftIO with
    static member inline (?<-) (x:IO<_>, _MonadIO:LiftIO, t:MaybeT<_>) = lift x
    static member inline (?<-) (x:IO<_>, _MonadIO:LiftIO, t:ListT<_> ) = lift x
    static member        (?<-) (x:IO<_>, _MonadIO:LiftIO, t:IO<_>    ) = x

let inline liftIO x : ^R = (x ? (LiftIO) <- Unchecked.defaultof< ^R>)


open Control.Monad.Cont

type CallCC = CallCC with
    static member inline (?<-) (f, _MonadCont:CallCC, t:MaybeT<_>) = MaybeT(callCC <| fun c -> runMaybeT(f (MaybeT << c << Some)))    
    static member inline (?<-) (f, _MonadCont:CallCC, t:ListT<_> ) = ListT (callCC <| fun c -> runListT (f (ListT << c << singleton)))
    static member (?<-) (f, _MonadCont:CallCC, t:Cont<_,_>) = callCC f

let inline callCC f : ^R = (f ? (CallCC) <- Unchecked.defaultof< ^R>)


open Control.Monad.State

type Get = Get with
    static member inline (?<-) (_ , _MonadState:Get, t:MaybeT<_>) = lift get
    static member inline (?<-) (_ , _MonadState:Get, t:ListT<_> ) = lift get

let inline get() : ^R = (Get ? (Get) <- Unchecked.defaultof< ^R> )

type Put = Put with
    static member inline (?<-) (_ , _MonadState:Put, t:MaybeT<_>) = lift << put
    static member inline (?<-) (_ , _MonadState:Put, t:ListT<_> ) = lift << put

let inline put x : ^R = (Put ? (Put) <- Unchecked.defaultof< ^R> ) x