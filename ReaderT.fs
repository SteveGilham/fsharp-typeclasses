module Control.Monad.ReaderT

open Prelude
open Control.Monad.Base
open Control.Monad.Reader
open Control.Monad.Trans

type ReaderT< ^r, ^m> = ReaderT of (^r -> ^m) with
    static member inline (?<-) (_      , _Functor:Fmap  , ReaderT m       ) = fun f -> ReaderT <| fun r -> do'{
        let! a = m r
        return (f a)}

let inline runReaderT (ReaderT x) = x
type ReaderT< ^r, ^m> with
    static member inline (?<-) (_        , _Monad:Return, _:ReaderT<_,_>) = fun a -> ReaderT <| fun _ -> return' a
    static member inline (?<-) (ReaderT m, _Monad:Bind  , _:ReaderT<_,_>) = fun k -> ReaderT <| fun r -> do'{
        let! a = m r
        return! runReaderT (k a) r}

    static member inline (?<-) (_        , _MonadPlus:Mzero, _:ReaderT<_,_>) = ReaderT <| fun _ -> mzero()
    static member inline (?<-) (ReaderT m, _MonadPlus:Mplus,   ReaderT n   ) = ReaderT <| fun r -> mplus (m r) (n r)

    static member inline (?<-) (m        , _MonadTrans:Lift, _:ReaderT<_,_>) = ReaderT <| fun _ -> m

    static member inline (?<-) (_        , _MonadReader:Ask   , _:ReaderT<_,_>) = ReaderT return'
    static member inline (?<-) (ReaderT m, _MonadReader:Local , _:ReaderT<_,_>) = fun f -> ReaderT(fun r -> m (f r))

    static member inline (?<-) (x:IO<_>  , _MonadIO :LiftIO   , _:ReaderT<_,_>) = lift x

    static member inline (?<-) (f        , _MonadCont :CallCC , _:ReaderT<_,_>) =
        ReaderT(fun r -> callCC <| fun c -> runReaderT (f (fun a -> ReaderT <| fun _ -> c a)) r)

    static member inline (?<-) (_        , _MonadState:Get    , _:ReaderT<_,_>) = lift get
    static member inline (?<-) (_        , _MonadState:Put    , _:ReaderT<_,_>) = lift << put

    static member inline (?<-) (_        , _MonadWriter:Tell  , _:ReaderT<_,_>) = lift tell
    static member inline (?<-) (ReaderT m, _MonadWriter:Listen, _:ReaderT<_,_>) = ReaderT <| fun w -> listen (m w)
    static member inline (?<-) (ReaderT m, _MonadWriter:Pass  , _:ReaderT<_,_>) = ReaderT <| fun w -> pass   (m w)

let inline  mapReaderT f (ReaderT m) = ReaderT(f << m)
let inline withReaderT f (ReaderT m) = ReaderT(m << f)