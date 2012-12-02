module Control.Monad.State

open Prelude
open Control.Applicative

type State<'S,'A> = State of ('S->('A * 'S)) with
    static member instance (_Functor:Fmap  , State m,              _) = fun f -> State(fun s -> let (a, s') = m s in (f a, s')) :State<'s,_>

let runState (State x) = x :'s->_
type State<'S,'A> with
    static member instance (_Monad:Return, _:State<'s,'a>           ) = fun a -> State(fun s -> (a, s))                                :State<'s,'a>
    static member instance (_Monad:Bind  ,   State m, _:State<'s,'b>) = fun k -> State(fun s -> let (a, s') = m s in runState(k a) s') :State<'s,'b>

type State<'S,'A> with
    static member instance (_Applicative:Pure, _:State<'s,'a>) = fun (x:'a) -> Applicative.pure' x :State<'s,_>
    static member instance (_Applicative:Ap, f:State<'s,_>, x:State<'s,'a>, _:State<'s,'b>) = fun () -> Applicative.ap f x :State<'s,'b>

let mapState  f (State m)  = State(f << m) :State<'s,_>
let withState f (State m)  = State(m << f) :State<'s,_>
let evalState (State sa) (s:'s) = fst(sa s)
let execState (State sa) (s:'s) = snd(sa s)
let get   = State (fun s -> (s , s))       :State<'s,_>
let put x = State (fun _ -> ((), x))       :State<'s,_>