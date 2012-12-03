module Control.Monad.Base
open Prelude

// MonadPlus class ------------------------------------------------------------
module MonadPlus =
    type Mzero = Mzero with
        static member instance (Mzero, _:option<'a>) = fun () -> None
        static member instance (Mzero, _:List<'a>  ) = fun () -> []

    type Mplus = Mplus with
        static member instance (Mplus, x:option<_>, _) = fun y -> match x with | None -> y | xs -> xs
        static member instance (Mplus, x:List<_>  , _) = fun y -> x @ y

let inline mzero () = Inline.instance MonadPlus.Mzero ()
let inline mplus (x:'a) (y:'a) : 'a = Inline.instance (MonadPlus.Mplus, x) y

let inline msum x =
    let foldR f s lst = List.foldBack f lst s
    foldR mplus (mzero()) x

let inline guard x = if x then return' () else mzero()

type DoPlusNotationBuilder() =
    member inline b.Return(x) = return' x
    member inline b.Bind(p,rest) = p >>= rest
    member b.Let(p,rest) = rest p
    member b.ReturnFrom(expr) = expr
    member inline x.Zero() = mzero()
    member inline x.Combine(a, b) = mplus a b
let doPlus = new DoPlusNotationBuilder()


let inline sequence ms =
    let k m m' = m >>= fun (x:'a) -> m' >>= fun xs -> (return' :list<'a> -> 'M) (List.Cons(x,xs))
    List.foldBack k ms ((return' :list<'a> -> 'M) [])

let inline mapM f as' = sequence (List.map f as')

let inline liftM  f m1    = m1 >>= (return' << f)
let inline liftM2 f m1 m2 = m1 >>= fun x1 -> m2 >>= fun x2 -> return' (f x1 x2)
let inline when'  p s     = if p then s else return' ()
let inline unless p s     = when' (not p) s
let inline ap     x y     = liftM2 id x y

let inline (>=>)  f g x   = f x >>= g
let inline (<=<)  g f x   = f x >>= g