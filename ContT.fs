module Control.Monad.ContT

open Prelude
open Control.Monad.Cont
open Control.Monad.Trans

type ContT< ^rma> = ContT of ^rma with
    static member inline fmap (Functor,   ContT m ) = fun f -> ContT(fun c -> m (c << f))

let inline runContT (ContT x) = x
type ContT< ^rma> with
    static member inline return' (Monad,          _:ContT<_>) = fun a -> ContT((|>) a)
    static member inline bind    (Monad, ContT m, _:ContT<_>) = fun k -> ContT(fun c -> m (fun a -> runContT(k a) c))

    static member inline lift   (MonadTrans, m, _:ContT<_>) = ContT((>>=) m)
    static member inline callCC (MonadCont , f, _:ContT<_>) = ContT(fun k -> runContT(f (fun a -> ContT(fun _ -> k a))) k)

    static member inline liftIO (MonadIO, x:IO<_>, _:ContT<_>) = lift x

    static member inline ask   (MonadReader,          _:ContT<_>) = lift ask
    static member inline local (MonadReader, ContT m, _:ContT<_>) = fun f -> ContT <| fun c -> do'{
        let! r = ask
        return! local f (m (local (const' r) << c))}
    
    static member inline get (MonadState, _:ContT<_>) = lift get
    static member inline put (MonadState, _:ContT<_>) = lift << put

let  mapContT f (ContT m) = ContT(f << m)
let withContT f (ContT m) = ContT(m << f)