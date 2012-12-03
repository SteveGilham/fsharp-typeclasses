module Control.Monad.ContT

open Prelude
open Control.Monad.Trans
open Control.Monad.Reader
open Control.Monad.State

type ContT<'Mr,'A> = ContT of  (('A -> 'Mr) -> 'Mr) with
    static member instance (Functor.Fmap, ContT m, _) = fun f -> ContT(fun c -> m (c << f))

let runContT (ContT x) = x

type ContT<'Mr,'A> with
    static member instance (Monad.Return, _:ContT<'mr,'a>           ) = fun a -> ContT((|>) a) :ContT<'mr,'a>
    static member instance (Monad.Bind  ,   ContT m, _:ContT<'mr,'b>) = fun k -> ContT(fun c -> m (fun a -> runContT(k a) c)) :ContT<'mr,'b>

    static member inline instance (MonadTrans.Lift  , _:ContT<'mr,'a>) = fun (m:'ma) -> ContT((>>=) m) : ContT<'mr,'a>
    static member        instance (MonadCont .CallCC, _:ContT<'mr,'b>) = fun f -> ContT(fun k -> runContT(f (fun a -> ContT(fun _ -> k a))) k) : ContT<'mr,'b>

    static member inline instance (MonadAsync.LiftAsync   , _:ContT<_,_>   ) = fun (x: Async<_>) -> lift (liftAsync x)

    static member instance (MonadReader.Ask, _:ContT<Reader<'a,'b>,'a>) = fun () -> lift ask :ContT<Reader<'a,'b>,'a>
    static member instance (MonadReader.Local, ContT m, _:ContT<Reader<'a,'b>,'t>) : ('a -> 'b) -> ContT<Reader<'a,'b>,'t> =
        fun f -> ContT <| fun c -> do'{     
            let! r = ask
            return! local f (m (local (const' r) << c))}
    
    static member instance (MonadState.Get   , _:ContT<State<'s,'a>,'s>  ) = fun () -> lift get:ContT<State<'s,'a>,'s>
    static member instance (MonadState.Put   , _:ContT<State<'s,'a>,unit>) = lift << put :'s -> ContT<State<'s,'a>,unit>

let  mapContT f (ContT m) = ContT(f << m)
let withContT f (ContT m) = ContT(m << f)