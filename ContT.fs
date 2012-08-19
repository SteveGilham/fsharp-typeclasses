module Control.Monad.ContT

open Prelude
open Control.Monad.Trans
open Control.Monad.Reader
open Control.Monad.State

type ContT<'Mr,'A> = ContT of  (('A -> 'Mr) -> 'Mr) with
    static member instance (_Functor:Fmap, ContT m, _) = fun f -> ContT(fun c -> m (c << f))

let runContT (ContT x) = x

type ContT<'Mr,'A> with
    static member instance (_Monad:Return, _:ContT<'mr,'a>           ) = fun a -> ContT((|>) a) :ContT<'mr,'a>
    static member instance (_Monad:Bind  ,   ContT m, _:ContT<'mr,'b>) = fun k -> ContT(fun c -> m (fun a -> runContT(k a) c)) :ContT<'mr,'b>

    static member inline instance (_MonadTrans:Lift  , _:ContT<'mr,'a>) = fun (m:'ma) -> ContT((>>=) m) : ContT<'mr,'a>
    static member        instance (_MonadCont :CallCC, _:ContT<'mr,'b>) = fun f -> ContT(fun k -> runContT(f (fun a -> ContT(fun _ -> k a))) k) : ContT<'mr,'b>

    static member inline instance (_MonadIO:LiftIO   , _:ContT<_,_>   ) = fun (x: IO<_>) -> lift (liftIO x)

    static member instance (_MonadReader:Ask, _:ContT<Reader<'a,'b>,'a>) = fun () -> lift ask :ContT<Reader<'a,'b>,'a>
    static member instance (_MonadReader:Local, ContT m, _:ContT<Reader<'a,'b>,'t>) : ('a -> 'b) -> ContT<Reader<'a,'b>,'t> =
        fun f -> ContT <| fun c -> do'{     
            let! r = ask
            return! local f (m (local (const' r) << c))}
    
    static member instance (_MonadState:Get   , _:ContT<State<'s,'a>,'s>  ) = fun () -> lift get:ContT<State<'s,'a>,'s>
    static member instance (_MonadState:Put   , _:ContT<State<'s,'a>,unit>) = lift << put :'s -> ContT<State<'s,'a>,unit>

let  mapContT f (ContT m) = ContT(f << m)
let withContT f (ContT m) = ContT(m << f)