module Control.Monad.StateT

open Prelude
open Control.Monad.Base
open Control.Monad.State
open Control.Monad.Trans

type StateT< ^s, ^m> = StateT of (^s -> ^m) with
    static member inline (?<-) (_     , _Functor:Fmap  , StateT m     ) = fun f -> StateT <| fun s -> do'{
        let! (x, s') = m s
        return (f x, s')}

let inline runStateT (StateT x) = x
type StateT< ^s, ^m> with
    static member inline (?<-) (_       , _Monad:Return, _:StateT<_,_>) = fun a -> StateT <| fun s -> return' (a, s)
    static member inline (?<-) (StateT m, _Monad:Bind  , _:StateT<_,_>) = fun k -> StateT <| fun s -> do'{
        let! (a, s') = m s
        return! runStateT (k a) s'}

    static member inline (?<-) (_       , _MonadPlus:Mzero, _:StateT<_,_>) = StateT <| fun _ -> mzero()
    static member inline (?<-) (StateT m, _MonadPlus:Mplus,   StateT n   ) = StateT <| fun s -> mplus (m s) (n s)

    static member inline (?<-) (m       , _MonadTrans:Lift, _:StateT<_,_>) = StateT <| fun s -> m >>= fun a -> return' (a,s)
    
    static member inline (?<-) (_       , _MonadState :Get, _:StateT<_,_>) =          StateT (fun s -> return' (s , s))
    static member inline (?<-) (_       , _MonadState :Put, _:StateT<_,_>) = fun x -> StateT (fun _ -> return' ((), x))

    static member inline (?<-) (x:IO<_> , _MonadIO :LiftIO, _:StateT<_,_>) = lift x

let inline  mapStateT f (StateT m) = StateT(f << m)
let inline withStateT f (StateT m) = StateT(m << f)