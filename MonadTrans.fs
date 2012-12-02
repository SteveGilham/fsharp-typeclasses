module Control.Monad.Trans

open Prelude
open Control.Monad.Base

let singleton x = [x]
let concat (x:List<List<'a>>) :List<'a> = List.concat x

module OptionT =

    type OptionT<'Ma> = OptionT of 'Ma with
        static member inline instance (_Functor:Fmap, OptionT x :OptionT<'ma>, _) = fun (f:'a->'b) -> OptionT (fmap (Option.map f) x) :OptionT<'mb>

    let inline runOptionT   (OptionT m) = m
    type OptionT<'Ma> with
        static member inline instance (_Monad:Return,            _:OptionT<'ma>) = OptionT << return' << Some :'a -> OptionT<'ma>
        static member inline instance (_Monad:Bind  , OptionT x :OptionT<'ma>, _:OptionT<'mb>) = 
            fun (f: 'a -> OptionT<'mb>) -> (OptionT <| do' {
                let! maybe_value = x
                return! match maybe_value with
                        | None    -> return' None
                        | Some value -> runOptionT <| f value}) :OptionT<'mb>

        static member inline instance (_MonadPlus:Mzero, _:OptionT<_>) = fun ()         -> OptionT (return' None)
        static member inline instance (_MonadPlus:Mplus, OptionT x, _) = fun (OptionT y) -> OptionT <| do' {
                let! maybe_value = x
                return! match maybe_value with
                        | None    -> y
                        | Some value -> x}

    let inline mapOptionT f (OptionT m) = OptionT (f m)


module ListT =

    type ListT<'Ma> = ListT of 'Ma with
        static member inline instance (_Functor:Fmap,             ListT x:ListT<'ma>, _) = fun (f:'a->'b) -> ListT (fmap (List.map f) x):ListT<'mb>

    let inline runListT (ListT m) = m
    type ListT<'Ma> with
        static member inline instance (_Monad:Return,                      _:ListT<'ma>) = ListT << return' << singleton :'a -> ListT<'ma>
        static member inline instance (_Monad:Bind  , ListT x:ListT<'ma>,  _:ListT<'mb>) =
            fun (k: 'a -> ListT<'mb>) -> 
                (ListT (x >>= mapM(runListT << k) >>= (concat >> return'))) :ListT<'mb>

        static member inline instance (_MonadPlus:Mzero, _:ListT<_>) = fun ()        -> ListT (return' [])
        static member inline instance (_MonadPlus:Mplus, ListT x, _) = fun (ListT y) -> ListT <| do' {
            let! a = x
            let! b = y
            return (a ++ b)}

    let inline mapListT f (ListT  m) = ListT (f m)

open OptionT
open ListT

type Lift = Lift with
    static member inline instance (_MonadTrans:Lift, _:OptionT<'m_a>) = OptionT << (liftM Some)      :'ma -> OptionT<'m_a>
    static member inline instance (_MonadTrans:Lift, _: ListT<'m_a> ) = ListT   << (liftM singleton) :'ma ->  ListT<'m_a> 

let inline lift (x:'ma) = Inline.instance Lift x

open Control.Applicative

type LiftAsync = LiftAsync with  
    static member inline instance (_MonadIO:LiftAsync, _:OptionT<'U>) = fun (x :Async<'a>) -> lift (Inline.instance LiftAsync x)
    static member inline instance (_MonadIO:LiftAsync, _:ListT< 'U> ) = fun (x :Async<'a>) -> lift (Inline.instance LiftAsync x)
    static member        instance (_MonadIO:LiftAsync, _:Async<'a>  ) = fun (x :Async<'a>) -> x

let inline liftAsync (x: Async<'a>) = Inline.instance LiftAsync x


open Control.Monad.Cont

type CallCC = CallCC with
    static member instance (_MonadCont:CallCC, _:OptionT<Cont<'r,option<'a>>>) = fun (f:((_ -> OptionT<Cont<_,'b>>) -> _)) -> OptionT(callCC <| fun c -> runOptionT(f (OptionT << c << Some)))     :OptionT<Cont<'r,option<'a>>>
    static member instance (_MonadCont:CallCC, _:ListT<Cont<'r,   List<'a>>> ) = fun (f:((_ -> ListT<Cont<_,'b>>  ) -> _)) -> ListT  (callCC <| fun c -> runListT  (f (ListT  << c << singleton))) :ListT<  Cont<'r,  List<'a>>>    
    static member instance (_MonadCont:CallCC, _:Cont<'r,'a>) = callCC : (('a -> Cont<'r,'b>) -> _) -> _

let inline callCC f = Inline.instance CallCC f


open Control.Monad.State

type Get = Get with
    static member inline instance (_MonadState:Get, _:OptionT<_>) = fun () -> lift get
    static member inline instance (_MonadState:Get, _:ListT<_>  ) = fun () -> lift get
    static member        instance (_MonadState:Get, _:State<_,_>) = fun () ->      get

let inline get() = Inline.instance Get ()

type Put = Put with
    static member inline instance (_MonadState:Put, _:OptionT<_>) = lift << put
    static member inline instance (_MonadState:Put, _:ListT<_>  ) = lift << put
    static member        instance (_MonadState:Put, _:State<_,_>) =         put

let inline put x = Inline.instance Put x


open Control.Monad.Reader

type Ask = Ask with
    static member instance (_MonadReader:Ask, _:OptionT<Reader<'a,option<'a>>>) = fun () -> lift ask :OptionT<Reader<'a,option<'a>>>
    static member instance (_MonadReader:Ask, _:ListT<Reader< 'a,List<   'a>>>) = fun () -> lift ask :  ListT<Reader<'a,  List<'a>>>
    static member instance (_MonadReader:Ask, _:Reader<'r,'r>                 ) = fun () ->      ask :Reader<'r,'r>

let inline ask() = Inline.instance Ask ()

type Local = Local with
    static member inline instance (_MonadReader:Local, OptionT m, _:OptionT<_> ) = fun f -> OptionT <| local f m
    static member inline instance (_MonadReader:Local, ListT   m, _: ListT<_>  ) = fun f -> ListT   <| local f m
    static member        instance (_MonadReader:Local,         m, _:Reader<_,_>) = fun f ->            local f m

let inline local f m = Inline.instance (Local, m) f


open Control.Monad.Writer

type Tell = Tell with
    static member inline instance (_MonadWriter:Tell, _:OptionT<_> ) = lift << tell
    static member        instance (_MonadWriter:Tell, _:Writer<_,_>) =         tell

let inline tell x = Inline.instance Tell x

type Listen = Listen with
    static member inline instance (_MonadWriter:Listen, m, _:OptionT<_> ) = fun () ->
        let liftMaybe (m,w) = Option.map (fun x -> (x,w)) m
        OptionT (listen (runOptionT m) >>= (return' << liftMaybe))
    static member        instance (_MonadWriter:Listen, m, _:Writer<_,_>) = fun () -> listen m

let inline listen m = Inline.instance (Listen, m) ()

type Pass = Pass with
    static member inline instance (_MonadWriter:Pass, m, _:OptionT<_> ) = fun () -> OptionT (runOptionT m >>= maybe (return' None) (liftM Some << pass << return'))
    static member        instance (_MonadWriter:Pass, m, _:Writer<_,_>) = fun () -> pass m

let inline pass m = Inline.instance (Pass, m) ()
