module Control.Monad.ReaderT

open Prelude
open Control.Monad.Base
open Control.Monad.Reader
open Control.Monad.Trans

type ReaderT< ^r, ^m> = ReaderT of (^r -> ^m) with
    static member inline (?<-) (_Functor:Fmap  , ReaderT m, _) = fun f -> ReaderT <| fun r -> do'{
        let! a = m r
        return (f a)}

let inline runReaderT (ReaderT x) = x
type ReaderT< ^r, ^m> with
    static member inline (?<-) (_Monad:Return, _:ReaderT<_,_>,         _) = fun a -> ReaderT <| fun _ -> return' a
    static member inline (?<-) (_Monad:Bind  , ReaderT m, _:ReaderT<_,_>) = fun k -> ReaderT <| fun r -> do'{
        let! a = m r
        return! runReaderT (k a) r}

    static member inline (?<-) (_MonadPlus:Mzero, _:ReaderT<_,_>, _) = ReaderT <| fun _ -> mzero()
    static member inline (?<-) (_MonadPlus:Mplus,   ReaderT m, ReaderT n) = ReaderT <| fun r -> mplus (m r) (n r)

    static member inline (?<-) (_MonadTrans:Lift, m        , _:ReaderT<_,_>) = ReaderT <| fun _ -> m

    static member inline (?<-) (_MonadReader:Ask   , _:ReaderT<_,_>, _) = ReaderT return'
    static member inline (?<-) (_MonadReader:Local , ReaderT m, _:ReaderT<_,_>) = fun f -> ReaderT(fun r -> m (f r))

    static member inline (?<-) (_MonadIO :LiftIO   , x:IO<_>  , _:ReaderT<_,_>) = lift x

    static member inline (?<-) (_MonadCont :CallCC , f        , _:ReaderT<_,_>) =
        ReaderT(fun r -> callCC <| fun c -> runReaderT (f (fun a -> ReaderT <| fun _ -> c a)) r)

    static member inline (?<-) (_MonadState:Get  , _:ReaderT<_,_>, _) = lift get
    static member inline (?<-) (_MonadState:Put  , _:ReaderT<_,_>, _) = lift << put

    static member inline (?<-) (_MonadWriter:Tell, _:ReaderT<_,_>, _) = lift tell
    static member inline (?<-) (_MonadWriter:Listen, ReaderT m, _:ReaderT<_,_>) = ReaderT <| fun w -> listen (m w)
    static member inline (?<-) (_MonadWriter:Pass  , ReaderT m, _:ReaderT<_,_>) = ReaderT <| fun w -> pass   (m w)

let inline  mapReaderT f (ReaderT m) = ReaderT(f << m)
let inline withReaderT f (ReaderT m) = ReaderT(m << f)