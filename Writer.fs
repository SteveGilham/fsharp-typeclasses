module Control.Monad.Writer

open Prelude
open Data.Monoid

type Writer<'W,'A> = Writer of ('A * 'W) with
    static member        instance (_Functor:Fmap,   Writer(a,w),                _) = fun f -> Writer(f a, w) :Writer<'w,_>

let runWriter (Writer x) = x :_*'w
type Writer<'W,'A> with
    static member inline instance (_Monad:Return, _:Writer<'w,'a>                ) = fun a -> Writer(a, mempty())                                       :Writer<'w,'a>
    static member inline instance (_Monad:Bind  ,   Writer(a, w), _:Writer<'w,'b>) = fun k -> Writer(let (b, w') = runWriter(k a) in (b, mappend w w')) :Writer<'w,'b>

let mapWriter f (Writer m:Writer<'w1,_>)   = Writer(f m) :Writer<'w2,_>
let execWriter  (Writer m:Writer<'w,_> ) s = snd m

let tell              w       = Writer((),     w)        :Writer<'w,_>
let listen(Writer (a, w))     = Writer((a, w), w)        :Writer<'w,_>
let pass  (Writer((a, f), w)) = Writer( a,   f w)        :Writer<'w,_>