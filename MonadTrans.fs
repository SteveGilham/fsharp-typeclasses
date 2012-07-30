module Control.Monad.Trans

open Prelude
open Control.Monad.Base
open Control.Applicative

let singleton x = [x]
let concat (x:List<List<'a>>) :List<'a> = List.concat x

module MaybeT =

    type MaybeT< ^ma > = MaybeT of (^ma ) with
        static member inline (?<-) (_       , _Functor:Fmap  ,   MaybeT x   ) = fun f -> MaybeT (fmap (Option.map f) x)

    let inline runMaybeT   (MaybeT m) = m
    type MaybeT< ^ma > with
        static member inline (?<-) (_       , _Monad  :Return, t:MaybeT<_>  ) = MaybeT << return' << Just
        static member inline (?<-) (MaybeT x, _Monad  :Bind  , t:MaybeT< ^b>) = fun (f: ^a -> MaybeT< ^b>) -> MaybeT <| do' {
            let! maybe_value = x
            return! match maybe_value with
                    | Nothing    -> return' Nothing
                    | Just value -> runMaybeT <| f value}

        static member inline (?<-) (f:MaybeT<_->_>, _Applicative:Ap   , _:MaybeT< ^b>) = fun x -> ap f x
        static member inline (?<-) (_             , _MonadPlus  :Mzero, _:MaybeT<_>  ) = MaybeT (return' Nothing)
        static member inline (?<-) (MaybeT x      , _MonadPlus  :Mplus,   MaybeT y   ) = MaybeT <| do' {
                let! maybe_value = x
                return! match maybe_value with
                        | Nothing    -> y
                        | Just value -> x}

    let inline mapMaybeT f (MaybeT m) = MaybeT (f m)


module ListT =

    type ListT< ^ma > = ListT of (^ma ) with
        static member inline (?<-) (_      , _Functor:Fmap  ,   ListT x   ) = fun f -> ListT (fmap (List.map f) x)

    let inline runListT (ListT m) = m
    type ListT< ^ma > with
        static member inline (?<-) (_      , _Monad  :Return, _:ListT<_>  ) = ListT << return' << singleton
        static member inline (?<-) (ListT x, _Monad  :Bind  , _:ListT< ^b>) = fun (k: ^a -> ^b ListT) -> 
            ListT (x >>= mapM(runListT << k)  >>= (concat >> return'))

        static member inline (?<-) (f:ListT<_->_>, _Applicative:Ap   , _:ListT< ^b>) = fun x -> ap f x
        static member inline (?<-) (_            , _MonadPlus  :Mzero, _:ListT<_>  ) = ListT (return' [])
        static member inline (?<-) (ListT x      , _MonadPlus  :Mplus,   ListT y   ) = ListT <| do' {
            let! a = x
            let! b = y
            return (a ++ b)}

    let inline mapListT f (ListT  m) = ListT (f m)

open MaybeT
open ListT

type Lift = Lift with
    static member inline (?<-) (x, _MonadTrans:Lift, _:MaybeT<_>) = MaybeT << (liftM Just)      <| x
    static member inline (?<-) (x, _MonadTrans:Lift, _:ListT<_> ) = ListT  << (liftM singleton) <| x

let inline lift x : ^R = x ? (Lift) <- defaultof< ^R>


type LiftIO = LiftIO with
    static member inline (?<-) (x:IO<_>, _MonadIO:LiftIO, _:MaybeT<_>) = lift x
    static member inline (?<-) (x:IO<_>, _MonadIO:LiftIO, _:ListT<_> ) = lift x
    static member        (?<-) (x:IO<_>, _MonadIO:LiftIO, _:IO<_>    ) =      x

let inline liftIO x : ^R = x ? (LiftIO) <- defaultof< ^R>


open Control.Monad.Cont

type CallCC = CallCC with
    static member inline (?<-) (f, _MonadCont:CallCC, _:MaybeT<_>) = MaybeT(callCC <| fun c -> runMaybeT(f (MaybeT << c << Just)))    
    static member inline (?<-) (f, _MonadCont:CallCC, _:ListT<_> ) = ListT (callCC <| fun c -> runListT (f (ListT  << c << singleton)))
    static member        (?<-) (f, _MonadCont:CallCC, _:Cont<_,_>) = callCC f

let inline callCC f : ^R = f ? (CallCC) <- defaultof< ^R>


open Control.Monad.State

type Get = Get with
    static member inline (?<-) (_, _MonadState:Get, _:MaybeT<_> ) = lift get
    static member inline (?<-) (_, _MonadState:Get, _:ListT<_>  ) = lift get
    static member        (?<-) (_, _MonadState:Get, _:State<_,_>) =      get

let inline get() : ^R = () ? (Get) <- defaultof< ^R>

type Put = Put with
    static member inline (?<-) (_, _MonadState:Put, _:MaybeT<_> ) = lift << put
    static member inline (?<-) (_, _MonadState:Put, _:ListT<_>  ) = lift << put
    static member        (?<-) (_, _MonadState:Put, _:State<_,_>) =         put

let inline put x : ^R = (() ? (Put) <- defaultof< ^R> ) x


open Control.Monad.Reader

type Ask = Ask with
    static member inline (?<-) (_, _MonadReader:Ask, _:MaybeT<_>  ) = lift ask
    static member inline (?<-) (_, _MonadReader:Ask, _:ListT<_>   ) = lift ask
    static member        (?<-) (_, _MonadReader:Ask, _:Reader<_,_>) =      ask

let inline ask() : ^R = () ? (Ask) <- defaultof< ^R>

type Local = Local with
    static member inline (?<-) (MaybeT m, _MonadReader:Local, _:MaybeT<_>  ) = fun f -> MaybeT <| local f m
    static member inline (?<-) (ListT  m, _MonadReader:Local, _:ListT<_>   ) = fun f -> ListT  <| local f m
    static member        (?<-) (m       , _MonadReader:Local, _:Reader<_,_>) = fun f ->           local f m

let inline local f m: ^R = (m ? (Local) <- defaultof< ^R>) f


open Control.Monad.Writer

type Tell = Tell with
    static member inline (?<-) (_, _MonadWriter:Tell, _:MaybeT<_>  ) = lift << tell
    static member        (?<-) (_, _MonadWriter:Tell, _:Writer<_,_>) =         tell

let inline tell x : ^R = (() ? (Tell) <- defaultof< ^R> ) x

type Listen = Listen with
    static member inline (?<-) (m, _MonadWriter:Listen, _:MaybeT<_>  ) =
        let liftMaybe (m,w) = Option.map (fun x -> (x,w)) m
        MaybeT (listen (runMaybeT m) >>= (return' << liftMaybe))
    static member        (?<-) (m, _MonadWriter:Listen, _:Writer<_,_>) = listen m

let inline listen m : ^R = m ? (Listen) <- defaultof< ^R>

type Pass = Pass with
    static member inline (?<-) (m, _MonadWriter:Pass, _:MaybeT<_>  ) = MaybeT (runMaybeT m >>= maybe (return' Nothing) (liftM Just << pass << return'))
    static member        (?<-) (m, _MonadWriter:Pass, _:Writer<_,_>) = pass m

let inline pass m : ^R = m ? (Pass) <- defaultof< ^R>