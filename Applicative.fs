module Control.Applicative

open Prelude
open Control.Monad.Base

type Applicative = Applicative with
    static member inline pure' x = return' x
    static member inline ap  f x = ap f x

type Pure = Pure with
    static member (?<-) (_Applicative:Pure, _:Maybe<'a>    , _) = fun (x:'a) -> Applicative.pure' x :Maybe<'a>
    static member (?<-) (_Applicative:Pure, _:List<'a>     , _) = fun (x:'a) -> Applicative.pure' x :List<'a> 
    static member (?<-) (_Applicative:Pure, _:IO<'a>       , _) = fun (x:'a) -> Applicative.pure' x :IO<'a>   
    static member (?<-) (_Applicative:Pure, _:'r -> 'a     , _) = const':'a  -> 'r -> _
    static member (?<-) (_Applicative:Pure, _:Either<'e,'a>, _) = fun (x:'a) -> Applicative.pure' x :Either<'e,_>

type Ap = Ap with
    static member (?<-) (_Applicative:Ap, x:Maybe<'a>    , _:Maybe<'b>    ) = fun (f:Maybe<_>    ) -> Applicative.ap f x :Maybe<'b>    
    static member (?<-) (_Applicative:Ap, x:List<'a>     , _:List<'b>     ) = fun (f:List<_>     ) -> Applicative.ap f x :List<'b>     
    static member (?<-) (_Applicative:Ap, x:IO<'a>       , _:IO<'b>       ) = fun (f:IO<_>       ) -> Applicative.ap f x :IO<'b>       
    static member (?<-) (_Applicative:Ap, g: _ -> 'a     , _: 'r -> 'b    ) = fun (f:'r -> _     ) -> fun x -> f x (g x) :'b     
    static member (?<-) (_Applicative:Ap, x:Either<'e,'a>, _:Either<'e,'b>) = fun (f:Either<'e,_>) -> Applicative.ap f x :Either<'e,'b>

let inline pure' x   = Inline.instance Pure x
let inline (<*>) f x = Inline.instance (Ap, x) f


type Empty = Empty with
    static member (?<-) (_Alternative:Empty, _:Maybe<'a>, _) = fun () -> Nothing
    static member (?<-) (_Alternative:Empty, _:List<'a> , _) = fun () -> []

let inline empty() = Inline.instance Empty ()


type Append = Append with    
    static member (?<-) (_Alternative:Append, x:Maybe<_>, _) = fun y -> match x with | Nothing -> y | xs -> xs
    static member (?<-) (_Alternative:Append, x:List<_> , _) = fun y -> x ++ y
    
let inline (<|>) (x:'a) (y:'a) :'a = Inline.instance (Append, x) y


let inline (<<|>) f a   = fmap f a
let inline liftA2 f a b = f <<|> a <*> b
let inline (  *>)   x   = x |> liftA2 (const' id)
let inline (<*  )   x   = x |> liftA2 const'
let inline (<**>)   x   = x |> liftA2 (|>)

let inline optional v = Just <<|> v <|> pure' Nothing

type ZipList<'s> = ZipList of 's seq with
    static member (?<-) (_Functor    :Fmap,   ZipList x  , _) = fun (f:'a->'b) -> ZipList (Seq.map f x)
    static member (?<-) (_Applicative:Pure, _:ZipList<'a>, _) = fun (x:'a)     -> ZipList (Seq.initInfinite (const' x))
    static member (?<-) (_Applicative:Ap  ,   ZipList x  , _:ZipList<'b>) = fun (ZipList (f:seq<'a->'b>)) ->
        ZipList (Seq.zip f x |> Seq.map (fun (f,x) -> f x)) :ZipList<'b>