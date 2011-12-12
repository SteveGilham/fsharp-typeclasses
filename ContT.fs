module Control.Monad.ContT

open Prelude
open Control.Monad.Cont
open Control.Monad.Trans

type ContT< ^rma> = ContT of ^rma with
    static member inline (?<-) (_      , _Functor   :Fmap  ,   ContT m ) = fun f -> ContT(fun c -> m (c << f))

let inline runContT (ContT x) = x
type ContT< ^rma> with
    static member inline (?<-) (_      , _Monad     :Return, _:ContT<_>) = fun a -> ContT((|>) a)
    static member inline (?<-) (ContT m, _Monad     :Bind  , _:ContT<_>) = fun k -> ContT(fun c -> m (fun a -> runContT(k a) c))

    static member inline (?<-) (m      , _MonadTrans:Lift  , _:ContT<_>) = ContT((>>=) m)
    static member inline (?<-) (f      , _MonadCont :CallCC, _:ContT<_>) = ContT(fun k -> runContT(f (fun a -> ContT(fun _ -> k a))) k)

    static member inline (?<-) (x:IO<_>, _MonadIO   :LiftIO, _:ContT<_>) = lift x

    static member inline (?<-) (_      , _MonadReader:Ask  , _:ContT<_>) = lift ask
    static member inline (?<-) (ContT m, _MonadReader:Local, _:ContT<_>) = fun f -> ContT <| fun c -> do'{
        let! r = ask
        return! local f (m (local (const' r) << c))}
    
    static member inline (?<-) (_      , _MonadState:Get   , _:ContT<_>) = lift get
    static member inline (?<-) (_      , _MonadState:Put   , _:ContT<_>) = lift << put

let  mapContT f (ContT m) = ContT(f << m)
let withContT f (ContT m) = ContT(m << f)