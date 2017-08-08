#r @"lib\InlineHelper.dll"
#load "Prelude.fs"
open Prelude

#load "Monoid.fs"
open Data.Monoid

#load "Monad.fs"
open Control.Monad.Base

#load "Applicative.fs"
open Control.Applicative

#load "Foldable.fs"
open Data.Foldable

#load "Traversable.fs"
open Data.Traversable


// Writer, including instances for Applicative.

type Writer<'W,'A> = Writer of ('A * 'W) with
    static member        instance (_Functor:Fmap,   Writer(a,w),                _) = fun f -> Writer(f a, w) :Writer<'w,_>

let runWriter (Writer x) = x :_*'w
type Writer<'W,'A> with
    static member inline instance (_Monad:Return, _:Writer<'w,'a>                ) = fun a -> Writer(a, mempty())                                       :Writer<'w,'a>
    static member inline instance (_Monad:Bind  ,   Writer(a, w), _:Writer<'w,'b>) = fun k -> Writer(let (b, w') = runWriter(k a) in (b, mappend w w')) :Writer<'w,'b>
    
    static member inline instance (_Applicative:Pure, _:Writer<'e,'a>) = fun (x:'a) -> Applicative.pure' x :Writer<'e,_>
    static member inline instance (_Applicative:Ap, f:Writer<'e,_>, x:Writer<'e,'a>, _:Writer<'e,'b>) = fun () -> Applicative.ap f x :Writer<'e,'b>


let mapWriter f (Writer m:Writer<'w1,_>)   = Writer(f m) :Writer<'w2,_>
let execWriter  (Writer m:Writer<'w,_> ) s = snd m

let tell              w       = Writer((),     w)        :Writer<'w,_>
let listen(Writer (a, w))     = Writer((a, w), w)        :Writer<'w,_>
let pass  (Writer((a, f), w)) = Writer( a,   f w)        :Writer<'w,_>



// Functor.Compose

type Compose<'fga> = Compose of 'fga with
    static member inline instance  (_Functor:Fmap, Compose x, _:Compose<_>) = fun f -> Compose (fmap (fmap f) x)

let getCompose(Compose (fga)) = fga 

type Compose<'fga> with
    static member inline instance (_Foldable   :FoldMap , Compose t, _) = fun f -> foldMap (foldMap f) t
    static member inline instance (_Traversable:Traverse, Compose t, _) = fun f -> Compose <<|> traverse (traverse f) t
    static member inline instance (_Applicative:Pure  , _:Compose<_>  ) = fun x -> Compose (pure' (pure' x))
    static member inline instance (_Applicative:Ap    ,   Compose f, Compose x, _:Compose<_>) = fun () -> Compose ((<*>) <<|> f <*> x)
    static member inline instance (_Alternative:Empty , _:Compose<'a> ) = fun () -> Compose ( empty() )
    static member inline instance (_Alternative:Append,   Compose x, _) = fun (Compose y) -> Compose (x <|> y)


let aaa = (+) <<|> Writer (2, ["a";"b"]) <*> Writer (3, ["c";"d"])  // [a,b,c,d], 5 -->  Writer (5, ["a"; "b"; "c"; "d"])
let inline mkp v a b = Compose (Writer (Writer (v,a), b))
let bbb = (+) <<|> mkp 2 [1;2] ["a";"b"] <*> mkp 3 [3;4] ["c";"d"]
let cbbb = getCompose bbb  // [a,b,c,d],([1,2,3,4],5)  --> Writer (Writer (5,[1,2,3,4]), [a,b,c,d])