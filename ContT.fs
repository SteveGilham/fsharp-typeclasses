module Control.Monad.ContT

open Prelude
open Control.Monad.Trans
open Control.Monad.Reader
open Control.Monad.State

type ContT<'Mr,'A> = ContT of  (('A -> 'Mr) -> 'Mr) with
    static member (?<-) (_Functor:Fmap, ContT m, _) = fun f -> ContT(fun c -> m (c << f))

let runContT (ContT x) = x

type ContT<'Mr,'A> with
    static member (?<-) (_Monad:Return, _:ContT<'mr,'a>,         _) = fun a -> ContT((|>) a) :ContT<'mr,'a>
    static member (?<-) (_Monad:Bind  ,   ContT m, _:ContT<'mr,'b>) = fun k -> ContT(fun c -> m (fun a -> runContT(k a) c)) :ContT<'mr,'b>

    static member inline (?<-) (_MonadTrans:Lift  , _:ContT<'mr,'a>, _) = fun (m:'ma) -> ContT((>>=) m) : ContT<'mr,'a>
    static member        (?<-) (_MonadCont :CallCC, _:ContT<'mr,'b>, _) = fun f -> ContT(fun k -> runContT(f (fun a -> ContT(fun _ -> k a))) k) : ContT<'mr,'b>

    static member inline (?<-) (_MonadIO:LiftIO, _:ContT<'mr2,'t2> , _) = fun (x: IO<'a2>) -> ((lift ((liftIO x) : 'ma2)) : 'R2)

    static member (?<-) (_MonadReader:Ask  , _:ContT<Reader<'a,'b>,'a>, _) = lift ask :ContT<Reader<'a,'b>,'a>
    static member (?<-) (_MonadReader:Local, ContT m, _:ContT<Reader<'a,'b>,'t>) : ('a -> 'b) -> ContT<Reader<'a,'b>,'t> =
        fun f -> ContT <| fun c -> do'{     
            let! r = ask
            return! local f (m (local (const' r) << c))}
    
    static member (?<-) (_MonadState:Get   , _:ContT<State<'s,'a>,'s>  , _) = lift    get :      ContT<State<'s,'a>,'s>
    static member (?<-) (_MonadState:Put   , _:ContT<State<'s,'a>,unit>, _) = lift << put :'s -> ContT<State<'s,'a>,unit>

let  mapContT f (ContT m) = ContT(f << m)
let withContT f (ContT m) = ContT(m << f)