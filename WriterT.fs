module Control.Monad.WriterT

open Prelude
open Control.Monad.Base
open Data.Monoid
open Control.Monad.Trans
open Control.Monad.Cont
open Control.Monad.Reader
open Control.Monad.State

type WriterT<'WMa> = WriterT of 'WMa with
    static member inline (?<-) (_Functor:Fmap  , WriterT m, _) = fun f -> WriterT <| do'{
        let! (a, w) = m
        return (f a, w)}

let inline runWriterT   (WriterT x) = x

type WriterT<'WMa> with
    static member inline (?<-) (_Monad:Return, _:WriterT<'wma>   , _              ) : 'a -> WriterT<'wma> = fun a -> WriterT (return' (a, mempty()))
    static member inline (?<-) (_Monad:Bind  ,   WriterT (m:'wma), _:WriterT<'wmb>) : ('a -> WriterT<'wmb>) -> WriterT<'wmb> =
        fun k -> WriterT <| do'{
            let! (a, w ) = m
            let! (b, w') = runWriterT (k a)
            return (b, w </mappend/> w')}

    static member inline (?<-) (_MonadPlus:Mzero , _:WriterT<_>, _       ) = fun ()          -> WriterT(mzero())
    static member inline (?<-) (_MonadPlus:Mplus ,   WriterT m,  _       ) = fun (WriterT n) -> WriterT(mplus m n)

    static member inline (?<-) (_MonadWriter:Tell, _:WriterT<_>,           _) = fun w -> WriterT(return' ((), w))
    static member inline (?<-) (_MonadWriter:Listen, WriterT m, _:WriterT<_>) = WriterT <| do'{
        let! (a, w) = m
        return ((a, w), w)}
    static member inline (?<-) (_MonadWriter:Pass  , WriterT m, _:WriterT<_>) = WriterT <| do' {
        let! ((a, f), w) = m
        return (a, f w)}

    static member inline (?<-) (_MonadTrans:Lift   , _:WriterT<'wma>,      _) : 'ma -> WriterT<'wma> = fun m -> WriterT <| do' {
        let! a = m
        return (a, mempty())}
    
    static member inline (?<-) (_MonadIO:LiftIO    , _:WriterT<_>,         _) = fun (x: IO<_>) -> lift (liftIO x)

    static member inline (?<-) (_MonadCont:CallCC  , _:WriterT<Cont<'r,'a*'b>>, _) : (('a->WriterT<Cont<'r,'t>>)->_) -> WriterT<Cont<'r,'a*'b>>= 
        fun f -> WriterT (callCC <| fun c -> runWriterT (f (fun a -> WriterT <| c (a, mempty()))))
    
    static member inline (?<-) (_MonadReader:Ask, _:WriterT<Reader<'a,'a*'b>>,        _) = fun () -> lift ask:WriterT<Reader<'a,'a*'b>>
    static member        (?<-) (_MonadReader:Local, WriterT m, _:WriterT<Reader<'a,'b>>)    : ('a->'t) -> WriterT<Reader<'a,'b>> = fun f -> WriterT(local f m)

    static member inline (?<-) (_MonadState:Get , _:WriterT<State<'a,'a*'b>>  , _) : unit -> WriterT<State<'a,'a*'b>>   = fun () -> lift get
    static member inline (?<-) (_MonadState:Put , _:WriterT<State<'a,unit*'b>>, _) :'a    -> WriterT<State<'a,unit*'b>> = lift << put
    

let inline mapWriterT f (WriterT m) = WriterT(f m)
let inline execWriter   (WriterT m) = do' {
    let! (_, w) = m
    return w}