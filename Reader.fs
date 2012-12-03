module Control.Monad.Reader

open Prelude
open Control.Applicative

type Reader<'R,'A> = Reader of ('R->'A) with
    static member instance (Functor.Fmap  , Reader m:Reader<'r,'a>, _) = fun (f:_->'b) -> Reader(fun r -> f (m r))

let runReader (Reader x) = x
type Reader<'R,'A> with
    static member instance (Monad.Return, _:Reader<'r,'a>            ) = fun a -> Reader(fun _ -> a)                    :Reader<'r,'a>
    static member instance (Monad.Bind  ,   Reader m, _:Reader<'r,'b>) = fun k -> Reader(fun r -> runReader(k (m r)) r) :Reader<'r,'b>

type Reader<'R,'A> with
    static member instance (Applicative.Pure, _:Reader<'r,'a>) = fun (x:'a) -> Applicative.pure' x :Reader<'r,_>
    static member instance (Applicative.Ap, f:Reader<'r,_>, x:Reader<'r,'a>, _:Reader<'r,'b>) = fun () -> Applicative.ap f x :Reader<'r,'b>

let mapReader  f (Reader m) = Reader(f << m) :Reader<'r,_>
let withReader f (Reader m) = Reader(m << f) :Reader<'r,_>
let ask                = Reader id
let local f (Reader m) = Reader(m << f)      :Reader<'r,_>