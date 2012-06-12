module Control.Monad.Writer

open Prelude
open Data.Monoid

type Writer<'w,'a> = Writer of ('a * 'w) with
    static member        fmap (Functor,   Writer(a,w)) = fun f -> Writer(f a, w)

let runWriter (Writer x) = x
type Writer<'w,'a> with
    static member inline return' (Monad, _:Writer<'s,'a>) = fun a -> Writer(a, mempty())                                       :Writer<'s,'a>
    static member inline bind (Monad,Writer(a, w), _:Writer<'s,'b>) = fun k -> Writer(let (b, w') = runWriter(k a) in (b, mappend w w')) :Writer<'s,'b>

let mapWriter f (Writer m)   = Writer(f m)
let execWriter  (Writer m) s = snd m

let tell              w       = Writer((),     w)
let listen(Writer (a, w))     = Writer((a, w), w)
let pass  (Writer((a, f), w)) = Writer( a,   f w)