module Control.Monad.ReaderT

open Prelude
open Control.Monad.Base
open Control.Monad.Trans
open Control.Monad.Cont
open Control.Monad.State
open Control.Monad.Writer

type ReaderT<'R,'Ma> = ReaderT of ('R -> 'Ma) with
    static member inline (?<-) (_Functor:Fmap  , ReaderT m    , _) = fun f -> ReaderT <| fun r -> do'{
        let! a = m r
        return (f a)}

let inline runReaderT (ReaderT x) = x

type ReaderT<'R,'Ma> with
    static member inline (?<-) (_Monad:Return, _:ReaderT<'r,'ma>,          _) :'a  -> ReaderT<'r,'ma> = fun a -> ReaderT <| fun _ -> return' a
    static member inline (?<-) (_Monad:Bind  ,   ReaderT m, _:ReaderT<'r,'m>) :('b -> ReaderT<'r,'m>) -> ReaderT<'r,'m> = 
        fun k -> ReaderT <| fun r -> do'{
            let! a = m r
            return! runReaderT (k a) r}

    static member inline (?<-) (_MonadPlus:Mzero, _:ReaderT<_,_>,      _) = fun ()          -> ReaderT <| fun _ -> mzero()
    static member inline (?<-) (_MonadPlus:Mplus,   ReaderT m   ,      _) = fun (ReaderT n) -> ReaderT <| fun r -> mplus (m r) (n r)

    static member inline (?<-) (_MonadTrans:Lift, _:ReaderT<'r,'ma>,   _) = fun m -> (ReaderT <| fun _ -> m) : ReaderT<'r,'ma>

    static member inline (?<-) (_MonadReader:Ask, _:ReaderT<'r,'a>,         _) = fun () -> ReaderT return' :ReaderT<'r,'a>
    static member inline (?<-) (_MonadReader:Local, ReaderT m, _:ReaderT<_,_>) = fun f  -> ReaderT(fun r -> m (f r))

    static member inline (?<-) (_MonadIO:LiftIO,  _:ReaderT<_,_>,           _) = fun (x: IO<_>) -> lift (liftIO x)

    static member (?<-) (_MonadCont :CallCC , _:ReaderT<'r,Cont<'c,'a>>, _) : (('a -> ReaderT<'t,Cont<'c,'u>>) -> ReaderT<'r,Cont<'c,'a>>) -> ReaderT<'r,Cont<'c,'a>> =
        fun f -> ReaderT(fun r -> callCC <| fun c -> runReaderT (f (fun a -> ReaderT <| fun _ -> c a)) r)

    static member (?<-) (_MonadState:Get    , _:ReaderT<'s,State<'a,'a>>  , _) = fun () -> lift get :ReaderT<'s,State<'a,'a>>
    static member (?<-) (_MonadState:Put    , _:ReaderT<'s,State<'a,unit>>, _) = lift << put : 'a -> ReaderT<'s,State<'a,unit>>

    static member (?<-) (_MonadWriter:Tell  , _:ReaderT<'t,'a->Writer<'a,unit>>,        _) :        ReaderT<'t,'a->Writer<'a,unit>> = lift tell
    static member (?<-) (_MonadWriter:Listen,   ReaderT m, _:ReaderT<'t,Writer<'a,'b*'a>>) :unit -> ReaderT<'t,Writer<'a,'b*'a>> = fun () -> ReaderT <| fun w -> listen (m w)  
    static member (?<-) (_MonadWriter:Pass  ,   ReaderT m, _:ReaderT<'t,Writer<'a,'b>>   ) :unit -> ReaderT<'t,Writer<'a,'b>>    = fun () -> ReaderT <| fun w -> pass   (m w)  

let inline  mapReaderT f (ReaderT m) = ReaderT(f << m)
let inline withReaderT f (ReaderT m) = ReaderT(m << f)