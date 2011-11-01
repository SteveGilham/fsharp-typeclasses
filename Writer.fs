module Control.Monad.Writer

open Prelude
open Data.Monoid

type Writer<'w,'a> = Writer of ('a * 'w) with
    static member ( ? ) (Writer (a,w), _Functor:Fmap)                 = fun f -> Writer (f a, w)

    static member inline (?<-) (_:Return    , _Monad  :Return, t:Writer<_,_>) = fun a -> Writer (a, mempty())
    static member inline (?<-) (Writer(a, w), _Monad  :Bind  , t:Writer<_,_>) =
        let runWriter (Writer x) = x
        fun k -> Writer (let (b, w') = runWriter (k a) in (b, mappend w w'))

let runWriter   (Writer x)   = x
let mapWriter f (Writer m)   = Writer (f m)
let execWriter  (Writer m) s = snd m

let tell              w       = Writer ((),     w)
let listen(Writer (a, w))     = Writer ((a, w), w)
let pass  (Writer((a, f), w)) = Writer ( a,   f w)