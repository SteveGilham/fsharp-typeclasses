module Control.Monad.Trans

open Prelude
open Control.Monad.Base
open Control.Applicative

let singleton x = [x]
let concat (x:List<List<'a>>) :List<'a> = List.concat x

module MaybeT =

    type MaybeT< ^ma > = MaybeT of (^ma ) with
        static member inline (?<-) (_Functor:Fmap  ,   MaybeT x, _) = fun f -> MaybeT (fmap (Option.map f) x)

    let inline runMaybeT   (MaybeT m) = m
    type MaybeT< ^ma > with
        static member inline (?<-) (_Monad:Return, _:MaybeT<_>,             _) = MaybeT << return' << Just
        static member inline (?<-) (_Monad:Bind,     MaybeT x, _:MaybeT< ^b> ) = fun (f: ^a -> MaybeT< ^b>) -> MaybeT <| do' {
            let! maybe_value = x
            return! match maybe_value with
                    | Nothing    -> return' Nothing
                    | Just value -> runMaybeT <| f value}

        static member inline (?<-) (_Applicative:Ap   , f:MaybeT<_->_>, _:MaybeT< ^b>) = fun x -> ap f x
        static member inline (?<-) (_MonadPlus  :Mzero, _:MaybeT<_>,       _) = MaybeT (return' Nothing)
        static member inline (?<-) (_MonadPlus  :Mplus,   MaybeT x, MaybeT y) = MaybeT <| do' {
                let! maybe_value = x
                return! match maybe_value with
                        | Nothing    -> y
                        | Just value -> x}

    let inline mapMaybeT f (MaybeT m) = MaybeT (f m)


module ListT =

    type ListT< ^ma > = ListT of (^ma ) with
        static member inline (?<-) (_Functor:Fmap  ,   ListT x, _) = fun f -> ListT (fmap (List.map f) x)

    let inline runListT (ListT m) = m
    type ListT< ^ma > with
        static member inline (?<-) (_Monad:Return, _:ListT<_>,           _) = ListT << return' << singleton
        static member inline (?<-) (_Monad:Bind    , ListT x, _:ListT< ^b>) = fun (k: ^a -> ^b ListT) -> 
            ListT (x >>= mapM(runListT << k)  >>= (concat >> return'))

        static member inline (?<-) (_Applicative:Ap   , f:ListT<_->_>, _:ListT< ^b>) = fun x -> ap f x
        static member inline (?<-) (_MonadPlus  :Mzero, _:ListT<_>,      _) = ListT (return' [])
        static member inline (?<-) (_MonadPlus  :Mplus,   ListT x, ListT y) = ListT <| do' {
            let! a = x
            let! b = y
            return (a ++ b)}

    let inline mapListT f (ListT  m) = ListT (f m)

open MaybeT
open ListT

type Lift = Lift with
    static member inline (?<-) (_MonadTrans:Lift, x, _:MaybeT<_>) = MaybeT << (liftM Just)      <| x
    static member inline (?<-) (_MonadTrans:Lift, x, _:ListT<_> ) = ListT  << (liftM singleton) <| x

let inline lift x : ^R = Lift ? (x) <- defaultof< ^R>


type LiftIO = LiftIO with
    static member inline (?<-) (_MonadIO:LiftIO, x:IO<_>, _:MaybeT<_>) = lift x
    static member inline (?<-) (_MonadIO:LiftIO, x:IO<_>, _:ListT<_> ) = lift x
    static member        (?<-) (_MonadIO:LiftIO, x:IO<_>, _:IO<_>    ) =      x

let inline liftIO x : ^R = LiftIO ? (x) <- defaultof< ^R>


open Control.Monad.Cont

type CallCC = CallCC with
    static member inline (?<-) (_MonadCont:CallCC, f, _:MaybeT<_>) = MaybeT(callCC <| fun c -> runMaybeT(f (MaybeT << c << Just)))    
    static member inline (?<-) (_MonadCont:CallCC, f, _:ListT<_> ) = ListT (callCC <| fun c -> runListT (f (ListT  << c << singleton)))
    static member        (?<-) (_MonadCont:CallCC, f, _:Cont<_,_>) = callCC f

let inline callCC f : ^R = CallCC ? (f) <- defaultof< ^R>


open Control.Monad.State

type Get = Get with
    static member inline (?<-) (_MonadState:Get, _:MaybeT<_> , _) = lift get
    static member inline (?<-) (_MonadState:Get, _:ListT<_>  , _) = lift get
    static member        (?<-) (_MonadState:Get, _:State<_,_>, _) =      get

let inline get() : ^R = Get ? (defaultof< ^R>) <- ()

type Put = Put with
    static member inline (?<-) (_MonadState:Put, _:MaybeT<_> , _) = lift << put
    static member inline (?<-) (_MonadState:Put, _:ListT<_>  , _) = lift << put
    static member        (?<-) (_MonadState:Put, _:State<_,_>, _) =         put

let inline put x : ^R = (Put ? (defaultof< ^R>) <- ()) x


open Control.Monad.Reader

type Ask = Ask with
    static member inline (?<-) (_MonadReader:Ask, _:MaybeT<_>  , _) = lift ask
    static member inline (?<-) (_MonadReader:Ask, _:ListT<_>   , _) = lift ask
    static member        (?<-) (_MonadReader:Ask, _:Reader<_,_>, _) =      ask

let inline ask() : ^R = Ask ? (defaultof< ^R>) <- ()

type Local = Local with
    static member inline (?<-) (_MonadReader:Local, MaybeT m, _:MaybeT<_>  ) = fun f -> MaybeT <| local f m
    static member inline (?<-) (_MonadReader:Local, ListT  m, _:ListT<_>   ) = fun f -> ListT  <| local f m
    static member        (?<-) (_MonadReader:Local, m       , _:Reader<_,_>) = fun f ->           local f m

let inline local f m: ^R = (Local ? (m) <- defaultof< ^R>) f


open Control.Monad.Writer

type Tell = Tell with
    static member inline (?<-) (_MonadWriter:Tell, _:MaybeT<_>  , _) = lift << tell
    static member        (?<-) (_MonadWriter:Tell, _:Writer<_,_>, _) =         tell

let inline tell x : ^R = (Tell ? (defaultof< ^R>) <- ()) x

type Listen = Listen with
    static member inline (?<-) (_MonadWriter:Listen, m, _:MaybeT<_>  ) =
        let liftMaybe (m,w) = Option.map (fun x -> (x,w)) m
        MaybeT (listen (runMaybeT m) >>= (return' << liftMaybe))
    static member        (?<-) (_MonadWriter:Listen, m, _:Writer<_,_>) = listen m

let inline listen m : ^R = Listen ? (m) <- defaultof< ^R>

type Pass = Pass with
    static member inline (?<-) (_MonadWriter:Pass, m, _:MaybeT<_>  ) = MaybeT (runMaybeT m >>= maybe (return' Nothing) (liftM Just << pass << return'))
    static member        (?<-) (_MonadWriter:Pass, m, _:Writer<_,_>) = pass m

let inline pass m : ^R = Pass ? (m) <- defaultof< ^R>