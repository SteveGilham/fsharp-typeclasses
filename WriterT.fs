module Control.Monad.WriterT

open Prelude
open Control.Monad
open Data.Monoid
open Control.Monad.State
open Control.Monad.Trans

type WriterT< ^sma> = WriterT of (^sma) with
    static member inline ( ? ) (WriterT m, _Functor:Fmap                 ) = fun f -> WriterT <| do'{
        let! (a, w) = m
        return (f a, w)}

    static member inline (?<-) (_:Return , _Monad :Return, t:WriterT<_>) = fun a -> WriterT (return' (a, mempty()))
    static member inline (?<-) (WriterT m, _Monad :Bind  , t:WriterT<_>) =
        let inline runWriterT (WriterT x) = x
        fun k -> WriterT <| do'{
            let! (a, w) = m
            let! (b, w') = runWriterT (k a)
            return (b,mappend w w')}

    static member inline (?<-) (_        , _MonadPlus:Mzero   , t:WriterT<_>) = WriterT (mzero())
    static member inline (?<-) (WriterT m, _MonadPlus:Mplus   , WriterT n   ) = WriterT (mplus m n)

    static member inline (?<-) (_        , _MonadWriter:Tell  , t:WriterT<_>) = fun w -> WriterT (return' ((), w))
    static member inline (?<-) (WriterT m, _MonadWriter:Listen, t:WriterT<_>) = WriterT <| do'{
        let! (a, w) = m
        return ((a, w), w)}
    static member inline (?<-) (WriterT m, _MonadWriter:Pass  , t:WriterT<_>) = WriterT <| do' {
        let! ((a, f), w) = m
        return (a, f w)}

    static member inline (?<-) (m        , _MonadTrans:Lift   , t:WriterT<_>) =  WriterT <| do' {
        let! a = m
        return (a, mempty())}
    
    static member inline (?<-) (x:IO<_>  , _MonadIO:LiftIO    , t:WriterT<_>) = lift x

    static member inline (?<-) (f        , _MonadCont :CallCC , t:WriterT<_>) = 
        let inline runWriterT (WriterT x) = x
        WriterT (callCC <| fun c -> runWriterT (f (fun a -> WriterT <| c (a, mempty()))))
    
    static member inline (?<-) (_        , _MonadState:Get    , t:WriterT<_>) = lift get
    static member inline (?<-) (_        , _MonadState:Put    , t:WriterT<_>) = lift << put
    

let inline mapWriterT f (WriterT m) = WriterT (f m)
let inline execWriter (WriterT m) = do' {
    let! (_, w) = m
    return w}
let inline runWriterT (WriterT x) = x