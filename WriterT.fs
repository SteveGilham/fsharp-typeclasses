module Control.Monad.WriterT

open Prelude
open Control.Monad.Base
open Data.Monoid
open Control.Monad.State
open Control.Monad.Trans

type WriterT< ^sma> = WriterT of (^sma) with
    static member inline fmap (Functor, WriterT m   ) = fun f -> WriterT <| do'{
        let! (a, w) = m
        return (f a, w)}

let inline runWriterT   (WriterT x) = x
type WriterT< ^sma> with
    static member inline return' (Monad, _:WriterT<_>) = fun a -> WriterT (return' (a, mempty()))
    static member inline bind (Monad, WriterT m, _:WriterT<_>) =
        fun k -> WriterT <| do'{
            let! (a, w ) = m
            let! (b, w') = runWriterT (k a)
            return (b,w </mappend/> w')}

    static member inline mzero (MonadPlus,          _:WriterT<_>) = WriterT(mzero())
    static member inline mplus (MonadPlus, WriterT m, WriterT n ) = WriterT(mplus m n)

    static member inline tell   (MonadWriter,            _:WriterT<_>) = fun w -> WriterT(return' ((), w))
    static member inline listen (MonadWriter, WriterT m, _:WriterT<_>) = WriterT <| do'{
        let! (a, w) = m
        return ((a, w), w)}
    static member inline pass   (MonadWriter, WriterT m, _:WriterT<_>) = WriterT <| do' {
        let! ((a, f), w) = m
        return (a, f w)}

    static member inline lift (MonadTrans, m, _:WriterT<_>) =  WriterT <| do' {
        let! a = m
        return (a, mempty())}
    
    static member inline liftIO (MonadIO, x:IO<_>, _:WriterT<_>) = lift x

    static member inline callCC (MonadCont, f, _:WriterT<_>) =
        WriterT (callCC <| fun c -> runWriterT (f (fun a -> WriterT <| c (a, mempty()))))
    
    static member inline ask   (MonadReader,            _:WriterT<_>) = lift ask
    static member inline local (MonadReader, WriterT m, _:WriterT<_>) = fun f -> WriterT(local f m)

    static member inline get (MonadState, _:WriterT<_>) = lift get
    static member inline put (MonadState, _:WriterT<_>) = lift << put
    

let inline mapWriterT f (WriterT m) = WriterT(f m)
let inline execWriter   (WriterT m) = do' {
    let! (_, w) = m
    return w}