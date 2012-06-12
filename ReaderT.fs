module Control.Monad.ReaderT

open Prelude
open Control.Monad.Base
open Control.Monad.Reader
open Control.Monad.Trans

type ReaderT< ^r, ^m> = ReaderT of (^r -> ^m) with
    static member inline fmap (Functor, ReaderT m       ) = fun f -> ReaderT <| fun r -> do'{
        let! a = m r
        return (f a)}

let inline runReaderT (ReaderT x) = x
type ReaderT< ^r, ^m> with
    static member inline return' (Monad, _:ReaderT<_,_>) = fun a -> ReaderT <| fun _ -> return' a
    static member inline bind    (Monad, ReaderT m, _:ReaderT<_,_>) = fun k -> ReaderT <| fun r -> do'{
        let! a = m r
        return! runReaderT (k a) r}

    static member inline mzero (MonadPlus, _:ReaderT<_,_>        ) = ReaderT <| fun _ -> mzero()
    static member inline mplus (MonadPlus,   ReaderT m, ReaderT n) = ReaderT <| fun r -> mplus (m r) (n r)

    static member inline lift (MonadTrans, m, _:ReaderT<_,_>) = ReaderT <| fun _ -> m

    static member inline ask   (MonadReader,            _:ReaderT<_,_>) = ReaderT return'
    static member inline local (MonadReader, ReaderT m, _:ReaderT<_,_>) = fun f -> ReaderT(fun r -> m (f r))

    static member inline liftIO (MonadIO, x:IO<_>, _:ReaderT<_,_>) = lift x

    static member inline callCC (MonadCont, f, _:ReaderT<_,_>) =
        ReaderT(fun r -> callCC <| fun c -> runReaderT (f (fun a -> ReaderT <| fun _ -> c a)) r)

    static member inline get (MonadState, _:ReaderT<_,_>) = lift get
    static member inline put (MonadState, _:ReaderT<_,_>) = lift << put

    static member inline tell   (MonadWriter,           _:ReaderT<_,_>) = lift tell
    static member inline listen (MonadWriter,ReaderT m, _:ReaderT<_,_>) = ReaderT <| fun w -> listen (m w)
    static member inline pass   (MonadWriter,ReaderT m, _:ReaderT<_,_>) = ReaderT <| fun w -> pass   (m w)

let inline  mapReaderT f (ReaderT m) = ReaderT(f << m)
let inline withReaderT f (ReaderT m) = ReaderT(m << f)