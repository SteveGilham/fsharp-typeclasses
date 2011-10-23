module Control.Monad

open Prelude

// MonadPlus class ------------------------------------------------------------

type Mzero = Mzero with
    static member (?<-) (_ , cs:Mzero, t:'a option) = None
    static member (?<-) (_ , cs:Mzero, t:'a list)   = []

let inline mzero () : ^R = (Mzero ? (Mzero) <- Unchecked.defaultof< ^R> )

type Mplus = Mplus with
    static member (?<-) (x:option<_> , cs:Mplus, y)       = 
            match x with
            | None -> y
            | xs   -> xs
    static member (?<-) (x:list<_> , cs:Mplus, y)       = List.append  x y

let inline mplus (x:'a) (y:'a) : 'a = x ? (Mplus) <- y

let inline msum x =
    let foldR f s lst = List.foldBack f lst s
    x |> foldR mplus (mzero())

let inline guard x = if x then return' () else mzero()

type DoPlusNotationBuilder() =
    member inline b.Return(x) = return' x
    member inline b.Bind(p,rest) = p >>= rest
    member b.Let(p,rest) = rest p
    member b.ReturnFrom(expr) = expr
    member inline x.Zero() = mzero()
    member inline x.Combine(a, b) = mplus a b
let doPlus = new DoPlusNotationBuilder()


let inline liftM  f m1      = do' { let! x1 = m1 in return (f x1) }
let inline liftM2 f m1 m2   = do' { let! x1 = m1 in let! x2 = m2 in return (f x1 x2) }
let inline when' p s        = if p then s else return' ()
let inline unless p s       = when' (not p) s
let inline ap x             = liftM2 id x

