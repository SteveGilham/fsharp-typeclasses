module Control.Monad.StateT

open Prelude
open Control.Monad.Base
open Control.Monad.Trans

type StateT<'S,'MaS> = StateT of ('S -> 'MaS) with
    static member inline (?<-) (_Functor:Fmap  , StateT m     ,_) = fun f -> StateT <| fun s -> do'{
        let! (x, s') = m s
        return (f x, s')}

let inline runStateT (StateT x) = x

type StateT<'S,'MaS> with
    static member inline (?<-) (_Monad:Return, _:StateT<'s,'ma>, _) : 'a -> StateT<'s,'ma> = fun a -> StateT <| fun s -> return' (a, s)
    static member inline (?<-) (_Monad:Bind  ,   StateT (m:'s->'mas), _:StateT<'s,'mbs>) :('a->StateT<'s,'mbs>) -> StateT<'s,'mbs> = 
        fun k -> StateT <| fun s -> do'{
            let! (a, s') = m s
            return! runStateT (k a) s'}

    static member inline (?<-) (_MonadPlus:Mzero, _:StateT<_,_>,     _) = StateT <| fun _ -> mzero()
    static member inline (?<-) (_MonadPlus:Mplus,   StateT m, StateT n) = StateT <| fun s -> mplus (m s) (n s)

    static member inline (?<-) (_MonadTrans:Lift, _:StateT<'s,'mas>, _) = fun (m:'ma) -> (StateT <| fun s -> m >>= fun a -> return' (a,s) ):StateT<'s,'mas>
    
    static member inline (?<-) (_MonadState :Get, _:StateT<_,_>,     _) =          StateT (fun s -> return' (s , s))
    static member inline (?<-) (_MonadState :Put, _:StateT<_,_>,     _) = fun x -> StateT (fun _ -> return' ((), x))

    static member inline (?<-) (_MonadIO:LiftIO, _:StateT<'s2,'t2>,  _) = fun (x: IO<'a2>) -> ((lift ((liftIO x) :'ma2)) :'R2)

let inline  mapStateT f (StateT m) = StateT(f << m)
let inline withStateT f (StateT m) = StateT(m << f)