module Control.Monad.State

open Prelude
type State<'s,'a> = State of ('s->('a * 's)) with
    static member (?<-) (_:unit , _Functor:Fmap  ,   State m   ) = fun f -> State (fun s -> let (a, s') = m s in (f a, s'))
    static member (?<-) (_:unit , _Monad  :Return, _:State<_,_>) = fun a -> State (fun s -> (a, s))
    static member (?<-) (State m, _Monad  :Bind  , _:State<_,_>) =
        let runState (State x) = x
        fun k -> State (fun s -> let (a, s') = m s in runState (k a) s')

let runState    (State x)  = x
let mapState  f (State m)  = State (f << m)
let withState f (State m)  = State (m << f)
let evalState (State sa) s = fst (sa s)
let execState (State sa) s = snd (sa s)
let get   = State (fun s -> (s , s))
let put x = State (fun _ -> ((), x))