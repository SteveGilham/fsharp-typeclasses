module Control.Monad.Writer

open Prelude
open Data.Monoid

type Writer<'w,'a> = Writer of ('a * 'w) with
    static member        (?<-) (_           , _Functor:Fmap  ,   Writer(a,w)) = fun f -> Writer(f a, w)

let runWriter (Writer x) = x
type Writer<'w,'a> with
    static member inline (?<-) (_           , _Monad  :Return, _:Writer<_,_>) = fun a -> Writer(a, mempty())
    static member inline (?<-) (Writer(a, w), _Monad  :Bind  , _:Writer<_,_>) = fun k -> Writer(let (b, w') = runWriter(k a) in (b, mappend w w'))

let mapWriter f (Writer m)   = Writer(f m)
let execWriter  (Writer m) s = snd m

let tell              w       = Writer((),     w)
let listen(Writer (a, w))     = Writer((a, w), w)
let pass  (Writer((a, f), w)) = Writer( a,   f w)