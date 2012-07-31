module Control.Monad.Trans

open Prelude
open Control.Monad.Base
open Control.Applicative

let singleton x = [x]
let concat (x:List<List<'a>>) :List<'a> = List.concat x

module MaybeT =

    type MaybeT< ^ma > = MaybeT of (^ma ) with
        static member inline fmap (Functor,   MaybeT x   ) = fun f -> MaybeT (fmap (Option.map f) x)

    let inline runMaybeT   (MaybeT m) = m
    type MaybeT< ^ma > with
        static member inline return' (Monad, t:MaybeT<_>  ) = MaybeT << return' << Just
        static member inline bind (Monad,MaybeT x, t:MaybeT< ^b>) = fun (f: ^a -> MaybeT< ^b>) -> MaybeT <| do' {
            let! maybe_value = x
            return! match maybe_value with
                    | Nothing    -> return' Nothing
                    | Just value -> runMaybeT <| f value}

        static member inline ap    (Applicative, f:MaybeT<_->_>, _:MaybeT< ^b>) = fun x -> ap f x
        static member inline mzero (MonadPlus, _:MaybeT<_>  ) = MaybeT (return' Nothing)
        static member inline mplus (MonadPlus, MaybeT x, MaybeT y   ) = MaybeT <| do' {
                let! maybe_value = x
                return! match maybe_value with
                        | Nothing -> y
                        | Just value -> x}

    let inline mapMaybeT f (MaybeT m) = MaybeT (f m)


module ListT =

    type ListT< ^ma > = ListT of (^ma ) with
        static member inline fmap (Functor,   ListT x   ) = fun f -> ListT (fmap (List.map f) x)

    let inline runListT (ListT m) = m
    type ListT< ^ma > with
        static member inline return' (Monad, _:ListT<_>  ) = ListT << return' << singleton
        static member inline bind (Monad, ListT x, _:ListT< ^b>) = fun (k: ^a -> ^b ListT) -> 
            ListT (x >>= mapM(runListT << k)  >>= (concat >> return'))

        static member inline ap  (Applicative, f:ListT<_->_>, _:ListT< ^b>) = fun x -> ap f x
        static member inline mzero (MonadPlus, _:ListT<_>  ) = ListT (return' [])
        static member inline mplus (MonadPlus, ListT x,   ListT y   ) = ListT <| do' {
            let! a = x
            let! b = y
            return (a ++ b)}

    let inline mapListT f (ListT  m) = ListT (f m)

open MaybeT
open ListT

#nowarn "64"

type MonadTrans = MonadTrans with
    static member inline lift (MonadTrans, x, _:MaybeT<_>) = MaybeT << (liftM Just)      <| x
    static member inline lift (MonadTrans, x, _: ListT<_>) = ListT  << (liftM singleton) <| x

let inline lift x : ^R = ((^C or ^a or ^R) : (static member lift  : ^C * ^a * ^R -> _) (MonadTrans, x, defaultof< ^R>))

type MonadIO = MonadIO with
    static member inline liftIO (MonadIO, x:IO<_>, _:MaybeT<_>) = lift x
    static member inline liftIO (MonadIO, x:IO<_>, _:ListT<_> ) = lift x
    static member        liftIO (MonadIO, x:IO<_>, _:IO<_>    ) =      x

let inline liftIO x : ^R = ((^C or ^a or ^R) : (static member liftIO  : ^C * ^a * ^R -> _) (MonadIO, x, defaultof< ^R>))


open Control.Monad.Cont

type MonadCont = MonadCont with
    static member inline callCC (MonadCont, f, _:MaybeT<_>) = MaybeT(callCC <| fun c -> runMaybeT(f (MaybeT << c << Just)))    
    static member inline callCC (MonadCont, f, _:ListT<_> ) = ListT (callCC <| fun c -> runListT (f (ListT  << c << singleton)))
    static member        callCC (MonadCont, f, _:Cont<_,_>) = callCC f

let inline callCC f : ^R = ((^C or ^a or ^R) : (static member callCC  : ^C * ^a * ^R -> _) (MonadCont, f, defaultof< ^R>))


open Control.Monad.State

type MonadState = MonadState with
    static member inline get (MonadState, _:MaybeT<_> ) = lift get
    static member inline get (MonadState, _:ListT<_>  ) = lift get
    static member        get (MonadState, _:State<_,_>) =      get

    static member inline put (MonadState, _:MaybeT<_> ) = lift << put
    static member inline put (MonadState, _:ListT<_>  ) = lift << put
    static member        put (MonadState, _:State<_,_>) =         put

let inline get() : ^R = ((^C or ^R) : (static member get  : ^C * ^R -> _) (MonadState, defaultof< ^R>))
let inline put x : ^R = ((^C or ^R) : (static member put  : ^C * ^R -> _) (MonadState, defaultof< ^R>)) x


open Control.Monad.Reader

type MonadReader = MonadReader with
    static member inline ask (MonadReader, _:MaybeT<_>  ) = lift ask
    static member inline ask (MonadReader, _:ListT<_>   ) = lift ask
    static member        ask (MonadReader, _:Reader<_,_>) =      ask

    static member inline local (MonadReader, MaybeT m, _:MaybeT<_>  ) = fun f -> MaybeT <| local f m
    static member inline local (MonadReader, ListT  m, _:ListT<_>   ) = fun f -> ListT  <| local f m
    static member        local (MonadReader, m       , _:Reader<_,_>) = fun f ->           local f m

let inline ask()    : ^R = ((^C or ^R)       : (static member ask  : ^C * ^R -> _     ) (MonadReader,    defaultof< ^R>))
let inline local f m: ^R = ((^C or ^a or ^R) : (static member local: ^C * ^a * ^R -> _) (MonadReader, m, defaultof< ^R>)) f


open Control.Monad.Writer

type MonadWriter = MonadWriter with
    static member inline tell (MonadWriter, _:MaybeT<_>  ) = lift << tell
    static member        tell (MonadWriter, _:Writer<_,_>) =         tell

    static member inline listen (MonadWriter, m, _:MaybeT<_>  ) =
        let liftMaybe (m,w) = Option.map (fun x -> (x,w)) m
        MaybeT (listen (runMaybeT m) >>= (return' << liftMaybe))
    static member        listen (MonadWriter, m, _:Writer<_,_>) = listen m

    static member inline pass (MonadWriter, m, _:MaybeT<_>  ) = MaybeT (runMaybeT m >>= maybe (return' Nothing) (liftM Just << pass << return'))
    static member        pass (MonadWriter, m, _:Writer<_,_>) = pass m


let inline tell x   : ^R = ((^C or ^R)       : (static member tell  : ^C * ^R -> _     ) (MonadWriter,    defaultof< ^R>)) x
let inline listen m : ^R = ((^C or ^a or ^R) : (static member listen: ^C * ^a * ^R -> _) (MonadWriter, m, defaultof< ^R>))
let inline pass   m : ^R = ((^C or ^a or ^R) : (static member pass  : ^C * ^a * ^R -> _) (MonadWriter, m, defaultof< ^R>))