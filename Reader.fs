module Control.Monad.Reader

open Prelude
type Reader<'R,'A> = Reader of ('R->'A) with
    static member instance (_Functor:Fmap  , Reader m:Reader<'r,'a>, _) = fun (f:_->'b) -> Reader(fun r -> f (m r))

let runReader (Reader x) = x
type Reader<'R,'A> with
    static member instance (_Monad:Return, _:Reader<'r,'a>            ) = fun a -> Reader(fun _ -> a)                    :Reader<'r,'a>
    static member instance (_Monad:Bind  ,   Reader m, _:Reader<'r,'b>) = fun k -> Reader(fun r -> runReader(k (m r)) r) :Reader<'r,'b>


let mapReader  f (Reader m) = Reader(f << m) :Reader<'r,_>
let withReader f (Reader m) = Reader(m << f) :Reader<'r,_>
let ask                = Reader id
let local f (Reader m) = Reader(m << f)      :Reader<'r,_>