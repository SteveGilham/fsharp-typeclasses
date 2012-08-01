module Control.Applicative

open Prelude
open Control.Monad.Base

type Applicative = Applicative with
    static member inline pure' x = return' x
    static member inline ap  f x = ap f x

type Pure = Pure with
    static member (?<-) (_, _Applicative:Pure, _:Maybe<'a>   ) = fun (x:'a) -> Applicative.pure' x :Maybe<'a>
    static member (?<-) (_, _Applicative:Pure, _:List<'a>    ) = fun (x:'a) -> Applicative.pure' x :List<'a> 
    static member (?<-) (_, _Applicative:Pure, _:IO<'a>      ) = fun (x:'a) -> Applicative.pure' x :IO<'a>   
    static member (?<-) (_, _Applicative:Pure, _:'r -> 'a    ) = const':'a  -> 'r -> _
    static member (?<-) (_, _Applicative:Pure, _:Either<'e,_>) = fun (x:'a) -> Applicative.pure' x :Either<'e,_>

let inline pure' x : ^R = (() ? (Pure) <- defaultof< ^R> ) x


type Ap = Ap with
    static member (?<-) (f:Maybe<_>    , _Applicative:Ap, x:Maybe<_>    ) = Applicative.ap f x
    static member (?<-) (f:List<_>     , _Applicative:Ap, x:List<_>     ) = Applicative.ap f x
    static member (?<-) (f:IO<_>       , _Applicative:Ap, x             ) = Applicative.ap f x
    static member (?<-) (f:_ -> _      , _Applicative:Ap, g: _ -> _     ) = fun x ->  f x (g x)
    static member (?<-) (f:Either<'e,_>, _Applicative:Ap, x:Either<'e,_>) = Applicative.ap f x

let inline (<*>) (x:'a) (y:'b) : 'c = x ? (Ap) <- y


type Empty = Empty with
    static member (?<-) (_, _Alternative:Empty, _:Maybe<'a>) = Nothing
    static member (?<-) (_, _Alternative:Empty, _:List<'a> ) = []

let inline empty() : ^R = () ? (Empty) <- defaultof< ^R>


type Append = Append with    
    static member (?<-) (x:Maybe<_>, _Alternative:Append, y) = match x with | Nothing -> y | xs -> xs
    static member (?<-) (x:List<_> , _Alternative:Append, y) = x ++ y
    
let inline (<|>) (x:'a) (y:'a) : 'a = x ? (Append) <- y



let inline (<<|>) f a   = fmap f a
let inline liftA2 f a b = f <<|> a <*> b
let inline (  *>)   x   = x |> liftA2 (const' id)
let inline (<*  )   x   = x |> liftA2 const'
let inline (<**>)   x   = x |> liftA2 (|>)

let inline optional v = Just <<|> v <|> pure' Nothing

type ZipList<'s> = ZipList of 's seq with
    static member (?<-) (_        , _Functor    :Fmap,   ZipList x  ) = fun f -> ZipList (Seq.map f x)
    static member (?<-) (_        , _Applicative:Pure, _:ZipList<'a>) = fun (x:'a) -> ZipList (Seq.initInfinite (const' x))
    static member (?<-) (ZipList f, _Applicative:Ap  ,   ZipList x  ) = ZipList (Seq.zip f x |> Seq.map (fun (f,x) -> f x))