module Control.Applicative

open Prelude
open Control.Monad.Base

type Pure = Pure with
    member inline this.Base                                  x = return' x
    static member (?<-) (_, _Applicative:Pure, _:'a option   ) = fun (x:'a) -> Pure.Pure.Base x :'a option
    static member (?<-) (_, _Applicative:Pure, _:'a list     ) = fun (x:'a) -> Pure.Pure.Base x :'a list
    static member (?<-) (_, _Applicative:Pure, _:'a IO       ) = fun (x:'a) -> Pure.Pure.Base x :'a IO
    static member (?<-) (_, _Applicative:Pure, _: _ -> 'a    ) = const'
    static member (?<-) (_, _Applicative:Pure, _:Either<'e,_>) = fun (x:'a) -> Pure.Pure.Base x :Either<'e,_>

let inline pure' x : ^R = (() ? (Pure) <- Unchecked.defaultof< ^R> ) x


type Ap = Ap with
    member inline this.Base                                     f x = ap f x
    static member (?<-) (f:option<_>, _Applicative:Ap, x:option<_>) = Ap.Ap.Base f x
    static member (?<-) (f:list<_>  , _Applicative:Ap, x:list<_>  ) = Ap.Ap.Base f x
    static member (?<-) (f:IO<_>    , _Applicative:Ap, x          ) = Ap.Ap.Base f x
    static member (?<-) (f:_ -> _   , _Applicative:Ap, g: _ -> _  ) = fun x ->   f x (g x)
    static member (?<-) (f:Either<'e,_>, _Applicative:Ap, x:Either<'e,_>) = Ap.Ap.Base f x

let inline (<*>) x y = x ? (Ap) <- y


type Empty = Empty with
    static member (?<-) (_, _Alternative:Empty, _:'a option) = None
    static member (?<-) (_, _Alternative:Empty, _:'a list  ) = []

let inline empty() : ^R = (() ? (Empty) <- Unchecked.defaultof< ^R>)


type Append = Append with    
    static member (?<-) (x:option<_>, _Alternative:Append, y) = match x with | None -> y | xs -> xs
    static member (?<-) (x:list<_>  , _Alternative:Append, y) = List.append  x y
    
let inline (<|>) (x:'a) (y:'a) : 'a = x ? (Append) <- y



let inline (<<|>) f a   = fmap f a
let inline liftA2 f a b = f <<|> a <*> b
let inline (  *>)   x   = x |> liftA2 (const' id)
let inline (<*  )   x   = x |> liftA2 const'
let inline (<**>)   x   = x |> liftA2 (|>)

let inline optional v = Some <<|> v <|> pure' None

type ZipList<'a> = ZipList of 'a seq with
    static member (?<-) (_        , _Functor    :Fmap,   ZipList x  ) = fun f -> ZipList (Seq.map f x)
    static member (?<-) (_        , _Applicative:Pure, _:ZipList<'a>) = fun x -> ZipList (Seq.initInfinite (const' x))
    static member (?<-) (ZipList f, _Applicative:Ap  ,   ZipList x  ) = ZipList (Seq.zip f x |> Seq.map (fun (f,x) -> f x))