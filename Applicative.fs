module Control.Applicative

open Prelude
open Control.Monad.Base

type Applicative = Applicative with
    static member inline pure' x = return' x
    static member inline ap  f x = ap f x

type Pure = Pure with
    static member (?<-) (_Applicative:Pure, _:Maybe<'a>   , _) = fun (x:'a) -> Applicative.pure' x :Maybe<'a>
    static member (?<-) (_Applicative:Pure, _:List<'a>    , _) = fun (x:'a) -> Applicative.pure' x :List<'a> 
    static member (?<-) (_Applicative:Pure, _:IO<'a>      , _) = fun (x:'a) -> Applicative.pure' x :IO<'a>   
    static member (?<-) (_Applicative:Pure, _:'r -> 'a    , _) = const':'a  -> 'r -> _
    static member (?<-) (_Applicative:Pure, _:Either<'e,_>, _) = fun (x:'a) -> Applicative.pure' x :Either<'e,_>

let inline pure' x : ^R = (Pure ? (defaultof< ^R>) <- ()) x


type Ap = Ap with
    static member (?<-) (_Applicative:Ap, f:Maybe<_>    , x:Maybe<_>    ) = Applicative.ap f x
    static member (?<-) (_Applicative:Ap, f:List<_>     , x:List<_>     ) = Applicative.ap f x
    static member (?<-) (_Applicative:Ap, f:IO<_>       , x             ) = Applicative.ap f x
    static member (?<-) (_Applicative:Ap, f:_ -> _      , g: _ -> _     ) = fun x ->  f x (g x)
    static member (?<-) (_Applicative:Ap, f:Either<'e,_>, x:Either<'e,_>) = Applicative.ap f x

let inline (<*>) (x:'a) (y:'b) : 'c = Ap ? (x) <- y


type Empty = Empty with
    static member (?<-) (_Alternative:Empty, _:Maybe<'a>, _) = Nothing
    static member (?<-) (_Alternative:Empty, _:List<'a> , _) = []

let inline empty() : ^R = Empty ? (defaultof< ^R>) <- ()


type Append = Append with    
    static member (?<-) (_Alternative:Append, x:Maybe<_>, y) = match x with | Nothing -> y | xs -> xs
    static member (?<-) (_Alternative:Append, x:List<_> , y) = x ++ y
    
let inline (<|>) (x:'a) (y:'a) : 'a = Append ? (x) <- y



let inline (<<|>) f a   = fmap f a
let inline liftA2 f a b = f <<|> a <*> b
let inline (  *>)   x   = x |> liftA2 (const' id)
let inline (<*  )   x   = x |> liftA2 const'
let inline (<**>)   x   = x |> liftA2 (|>)

let inline optional v = Just <<|> v <|> pure' Nothing

type ZipList<'s> = ZipList of 's seq with
    static member (?<-) (_Functor    :Fmap,   ZipList x  ,       _) = fun f -> ZipList (Seq.map f x)
    static member (?<-) (_Applicative:Pure, _:ZipList<'a>,       _) = fun (x:'a) -> ZipList (Seq.initInfinite (const' x))
    static member (?<-) (_Applicative:Ap  ,   ZipList f, ZipList x) = ZipList (Seq.zip f x |> Seq.map (fun (f,x) -> f x))