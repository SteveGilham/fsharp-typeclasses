module Control.Monad.ContT

open Prelude
open Control.Monad.Cont
open Control.Monad.Trans

type ContT< ^rma> = ContT of ^rma with
    static member inline ( ? ) (ContT m , _Functor:Fmap              ) = fun f -> ContT <| fun c -> m (c << f)

    static member inline (?<-) (_:Return, _Monad  :Return, t:ContT<_>) = fun a -> ContT ((|>) a)
    static member inline (?<-) (ContT m , _Monad  :Bind  , t:ContT<_>     ) =
        let inline runContT (ContT x) = x
        fun k -> ContT <| fun c -> m (fun a -> runContT (k a) c)

    static member inline (?<-) (m       , _MonadTrans:Lift  , t:ContT<_>) = ContT ((>>=) m)

    static member inline (?<-) (f       , _MonadCont :CallCC, t:ContT<_>) = 
        let inline runContT (ContT x) = x
        ContT <| fun k -> runContT (f (fun a -> ContT <| fun _ -> k a)) k

    static member inline (?<-) (x:IO<_> , _MonadIO:LiftIO   , t:ContT<_>) = lift x

    static member inline (?<-) (_       , _MonadState:Get   , t:ContT<_>) = lift get
    static member inline (?<-) (_       , _MonadState:Put   , t:ContT<_>) = lift << put

let mapContT      f (ContT m) = ContT (f << m)
let withContT     f (ContT m) = ContT (m << f)
let inline runContT (ContT x) = x