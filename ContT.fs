module Control.Monad.ContT

open Prelude
open Control.Monad.Cont
open Control.Monad.Trans

type ContT< ^rma> = ContT of ^rma with
    static member inline (?<-) (_Functor:Fmap,   ContT m, _) = fun f -> ContT(fun c -> m (c << f))

let inline runContT (ContT x) = x
type ContT< ^rma> with
    static member inline (?<-) (_Monad:Return, _:ContT<_>, _         ) = fun a -> ContT((|>) a)
    static member inline (?<-) (_Monad:Bind  ,   ContT m , _:ContT<_>) = fun k -> ContT(fun c -> m (fun a -> runContT(k a) c))

    static member inline (?<-) (_MonadTrans:Lift  , m      , _:ContT<_>) = ContT((>>=) m)
    static member inline (?<-) (_MonadCont :CallCC, f      , _:ContT<_>) = ContT(fun k -> runContT(f (fun a -> ContT(fun _ -> k a))) k)

    static member inline (?<-) (_MonadIO   :LiftIO, x:IO<_>, _:ContT<_>) = lift x

    static member inline (?<-) (_MonadReader:Ask  ,       _:ContT<_>, _) = lift ask
    static member inline (?<-) (_MonadReader:Local, ContT m, _:ContT<_>) = fun f -> ContT <| fun c -> do'{
        let! r = ask
        return! local f (m (local (const' r) << c))}
    
    static member inline (?<-) (_MonadState:Get   , _:ContT<_>, _) = lift get
    static member inline (?<-) (_MonadState:Put   , _:ContT<_>, _) = lift << put

let  mapContT f (ContT m) = ContT(f << m)
let withContT f (ContT m) = ContT(m << f)