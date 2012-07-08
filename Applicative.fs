module Control.Applicative

open Prelude
open Control.Monad.Base

#nowarn "64"

type Applicative = Applicative with
    static member inline pure'                        x = return' x
    static member pure' (_:Applicative, _:'a option   ) = fun (x:'a) -> Applicative.pure' x :'a option
    static member pure' (_:Applicative, _:'a list     ) = fun (x:'a) -> Applicative.pure' x :'a list
    static member pure' (_:Applicative, _:'a IO       ) = fun (x:'a) -> Applicative.pure' x :'a IO
    static member pure' (_:Applicative, _: _ -> 'a    ) = const'
    static member pure' (_:Applicative, _:Either<'e,_>) = fun (x:'a) -> Applicative.pure' x :Either<'e,_>

    static member inline ap f = fun x -> ap f x
    static member ap (_:Applicative, f:option<_>   , x:option<_>   ) = Applicative.ap f x
    static member ap (_:Applicative, f:list<_>     , x:list<_>     ) = Applicative.ap f x
    static member ap (_:Applicative, f:IO<_>       , x             ) = Applicative.ap f x
    static member ap (_:Applicative, f:_ -> _      , g: _ -> _     ) = fun x ->  f x (g x)
    static member ap (_:Applicative, f:Either<'e,_>, x:Either<'e,_>) = Applicative.ap f x

let inline pure' x : ^R = ((^C or ^R) : (static member pure': ^C * ^R -> _) (Applicative, defaultof< ^R>)) x
let inline (<*>) x y = ((^C or ^a or ^b) : (static member ap: ^C * ^a * ^b -> _) (Applicative, x, y))


type Alternative = Alternative with
    static member empty (Alternative, _:'a option) = None
    static member empty (Alternative, _:'a list  ) = []

    static member append (Alternative, x:option<_>, y) = match x with | None -> y | xs -> xs
    static member append (Alternative, x:list<_>  , y) = List.append  x y

let inline empty() : ^R = ((^C or ^R) : (static member empty: ^C * ^R -> _) (Alternative, defaultof< ^R>))
let inline (<|>) (x:^a) (y:^a) : ^a = ((^C or ^a) : (static member append: ^C * ^a * ^a -> _) (Alternative, x, y))



let inline (<<|>) f a   = fmap f a
let inline liftA2 f a b = f <<|> a <*> b
let inline (  *>)   x   = x |> liftA2 (const' id)
let inline (<*  )   x   = x |> liftA2 const'
let inline (<**>)   x   = x |> liftA2 (|>)

let inline optional v = Some <<|> v <|> pure' None

type ZipList<'a> = ZipList of 'a seq with
    static member fmap (Functor, ZipList x     ) = fun f -> ZipList (Seq.map f x)
    static member pure' (Applicative, _:ZipList<'a>) = fun x -> ZipList (Seq.initInfinite (const' x))
    static member ap    (Applicative, ZipList f, ZipList x  ) = ZipList (Seq.zip f x |> Seq.map (fun (f,x) -> f x))