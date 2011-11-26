module Control.Monad.WriterT

open Prelude
open Control.Monad.Base
open Data.Monoid
open Control.Monad.State
open Control.Monad.Trans

type WriterT< ^sma> = WriterT of (^sma) with
    static member inline (?<-) (_, _Functor:Fmap, WriterT m) = fun f -> WriterT <| do'{
        let! (a, w) = m
        return (f a, w)}
    static member inline (?<-) (_        , _Monad:Return, _:WriterT<_>) = fun a -> WriterT (return' (a, mempty()))
    static member inline (?<-) (WriterT m, _Monad:Bind  , _:WriterT<_>) =
        let inline runWriterT (WriterT x) = x
        fun k -> WriterT <| do'{
            let! (a, w ) = m
            let! (b, w') = runWriterT (k a)
            return (b,mappend w w')}

    static member inline (?<-) (_        , _MonadPlus  :Mzero , _:WriterT<_>) = WriterT (mzero())
    static member inline (?<-) (WriterT m, _MonadPlus  :Mplus ,   WriterT n ) = WriterT (mplus m n)

    static member inline (?<-) (_        , _MonadWriter:Tell  , _:WriterT<_>) = fun w -> WriterT (return' ((), w))
    static member inline (?<-) (WriterT m, _MonadWriter:Listen, _:WriterT<_>) = WriterT <| do'{
        let! (a, w) = m
        return ((a, w), w)}
    static member inline (?<-) (WriterT m, _MonadWriter:Pass  , _:WriterT<_>) = WriterT <| do' {
        let! ((a, f), w) = m
        return (a, f w)}

    static member inline (?<-) (m        , _MonadTrans:Lift   , _:WriterT<_>) =  WriterT <| do' {
        let! a = m
        return (a, mempty())}
    
    static member inline (?<-) (x:IO<_>  , _MonadIO   :LiftIO , _:WriterT<_>) = lift x

    static member inline (?<-) (f        , _MonadCont :CallCC , _:WriterT<_>) = 
        let inline runWriterT (WriterT x) = x
        WriterT (callCC <| fun c -> runWriterT (f (fun a -> WriterT <| c (a, mempty()))))
    
    static member inline (?<-) (_        , _MonadState:Get    , _:WriterT<_>) = lift get
    static member inline (?<-) (_        , _MonadState:Put    , _:WriterT<_>) = lift << put
    

let inline mapWriterT f (WriterT m) = WriterT (f m)
let inline execWriter (WriterT m) = do' {
    let! (_, w) = m
    return w}
let inline runWriterT (WriterT x) = x