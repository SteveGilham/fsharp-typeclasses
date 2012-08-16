module Control.Monad.WriterT

open Prelude
open Control.Monad.Base
open Data.Monoid
open Control.Monad.State
open Control.Monad.Trans

type WriterT<'sma> = WriterT of ('sma) with
    static member inline (?<-) (_Functor:Fmap  , WriterT m, _) = fun f -> WriterT <| do'{
        let! (a, w) = m
        return (f a, w)}

let inline runWriterT   (WriterT x) = x
type WriterT<'sma> with
    static member inline (?<-) (_Monad  :Return, _:WriterT<_>,           _) = fun a -> WriterT (return' (a, mempty()))
    static member inline (?<-) (_Monad  :Bind  ,   WriterT m, _:WriterT<_>) =
        fun k -> WriterT <| do'{
            let! (a, w ) = m
            let! (b, w') = runWriterT (k a)
            return (b, w </mappend/> w')}

    static member inline (?<-) (_MonadPlus  :Mzero , _:WriterT<_>,         _) = WriterT(mzero())
    static member inline (?<-) (_MonadPlus  :Mplus ,   WriterT m, WriterT n ) = WriterT(mplus m n)

    static member inline (?<-) (_MonadWriter:Tell  , _:WriterT<_>,         _) = fun w -> WriterT(return' ((), w))
    static member inline (?<-) (_MonadWriter:Listen, WriterT m, _:WriterT<_>) = WriterT <| do'{
        let! (a, w) = m
        return ((a, w), w)}
    static member inline (?<-) (_MonadWriter:Pass  , WriterT m, _:WriterT<_>) = WriterT <| do' {
        let! ((a, f), w) = m
        return (a, f w)}

    static member inline (?<-) (_MonadTrans:Lift   , m        , _:WriterT<_>) =  WriterT <| do' {
        let! a = m
        return (a, mempty())}
    
    static member inline (?<-) (_MonadIO   :LiftIO , x:IO<_>  , _:WriterT<_>) = lift x

    static member inline (?<-) (_MonadCont :CallCC , f        , _:WriterT<_>) =
        WriterT (callCC <| fun c -> runWriterT (f (fun a -> WriterT <| c (a, mempty()))))
    
    static member inline (?<-) (_MonadReader:Ask  , _:WriterT<_>, ()) = lift ask
    static member inline (?<-) (_MonadReader:Local,   WriterT m, _:WriterT<_>) = fun f -> WriterT(local f m)

    static member inline (?<-) (_MonadState:Get   , _:WriterT<_>, _) = lift get
    static member inline (?<-) (_MonadState:Put   , _:WriterT<_>, _) = lift << put
    

let inline mapWriterT f (WriterT m) = WriterT(f m)
let inline execWriter   (WriterT m) = do' {
    let! (_, w) = m
    return w}