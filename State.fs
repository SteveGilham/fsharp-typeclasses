module Control.Monad.State

open Prelude
type State<'s,'a> = State of ('s->('a * 's)) with
    static member fmap (Functor,   State m   ) = fun f -> State(fun s -> let (a, s') = m s in (f a, s'))


let runState (State x) = x
type State<'s,'a> with
    static member return' (Monad, _:State<'s,'a>) = fun a -> State(fun s -> (a, s))                                :State<'s,'a>
    static member bind (Monad, State m, _:State<'s,'b>) = fun k -> State(fun s -> let (a, s') = m s in runState(k a) s') :State<'s,'b>

let mapState  f (State m)  = State(f << m)
let withState f (State m)  = State(m << f)
let evalState (State sa) s = fst(sa s)
let execState (State sa) s = snd(sa s)
let get   = State (fun s -> (s , s))
let put x = State (fun _ -> ((), x))