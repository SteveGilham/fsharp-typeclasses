module Control.Monad.IO

open Prelude
open Control.Monad.Trans
open Control.Monad.Trans.Maybe
open Control.Monad.Trans.List

type LiftIO = LiftIO with
    static member inline (?<-) (x:IO<_>, _MonadIO:LiftIO, t:MaybeT<_>) = lift x
    static member inline (?<-) (x:IO<_>, _MonadIO:LiftIO, t:ListT<_> ) = lift x

    static member (?<-) (x:IO<_>, _MonadIO:LiftIO, t:IO<_>) = x

let inline liftIO x : ^R = (x ? (LiftIO) <- Unchecked.defaultof< ^R>)

