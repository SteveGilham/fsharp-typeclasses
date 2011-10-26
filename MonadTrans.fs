module Control.Monad.Trans

open Prelude

let ListSingleton x = [x]
let concat (x:'a list list) :'a list = List.concat x

type MaybeT< ^ma > = MaybeT of (^ma ) with
    static member inline (?<-) (_:Return, cs:Return, t:MaybeT<_>) = MaybeT << return' << Some

    static member inline (?) (MaybeT x, cs:Bind) =
        let runMaybeT (MaybeT m) = m
        fun f ->
            MaybeT <| do' {
                let! maybe_value = x
                return! match maybe_value with
                        | None       -> return' None
                        | Some value -> runMaybeT <| f value}

    static member inline (?<-) (_ , cs:Mzero, t:MaybeT<_>)   = MaybeT <| return' None

    static member inline (?<-) (MaybeT x  , cs:Mplus,MaybeT y) =
        MaybeT <| do' {
            let! maybe_value = x
            return! 
                match maybe_value with
                | None -> y
                | Some value -> x}

type ListT< ^ma > = ListT of (^ma ) with
    static member inline (?<-) (_:Return, cs:Return, t:ListT<_>) = MaybeT << return' << ListSingleton

    static member inline (?) (ListT (x: ^A), cs:Bind) =
        let inline runListT (ListT m) = m
        fun k ->
            let inline runListT (ListT l) = l
            ListT ( x >>= mapM (  (runListT) << k)  >>= (concat >> return') )

    static member inline (?<-) (_ , cs:Mzero, t:ListT<_>)   = ListT <| return' []

    static member inline (?<-) (ListT x  , cs:Mplus,ListT y) =
        ListT <| do' {
            let! a = x
            let! b = y
            return (a @ b)}

type Lift = Lift with
    static member inline (?<-) (x, cs:Lift, t:MaybeT<_>)  = MaybeT << (liftM Some)          <| x
    static member inline (?<-) (x, cs:Lift, t: ListT<_>)  = ListT  << (liftM ListSingleton) <| x

let inline lift x : ^R = (x ? (Lift) <- Unchecked.defaultof< ^R>)

let inline runMaybeT (MaybeT m)  = m
let inline runListT  (ListT  m)  = m