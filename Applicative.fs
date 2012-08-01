module Control.Applicative

open Prelude
open Control.Monad.Base

#nowarn "64"

type Applicative = Applicative with
    static member inline pure'                        x = return' x
    static member pure' (_:Applicative, _:Maybe<'a>   ) = fun (x:'a) -> Applicative.pure' x :Maybe<'a>
    static member pure' (_:Applicative, _:List<'a>    ) = fun (x:'a) -> Applicative.pure' x :List<'a> 
    static member pure' (_:Applicative, _:IO<'a>      ) = fun (x:'a) -> Applicative.pure' x :IO<'a>   
    static member pure' (_:Applicative, _: _ -> 'a    ) = const'
    static member pure' (_:Applicative, _:Either<'e,_>) = fun (x:'a) -> Applicative.pure' x :Either<'e,_>

    static member inline ap f = fun x -> ap f x
    static member ap (_:Applicative, f:Maybe<_>    , x:Maybe<'a>    , _:Maybe<'b>    ) = Applicative.ap f x 
    static member ap (_:Applicative, f:List<_>     , x:List<'a>     , _:List<'b>     ) = Applicative.ap f x 
    static member ap (_:Applicative, f:IO<_>       , x:IO<'a>       , _:IO<'b>       ) = Applicative.ap f x 
    static member ap (_:Applicative, f:_ -> _      , g: _ -> 'a     , _: _ -> 'b     ) = fun x ->  f x (g x)
    static member ap (_:Applicative, f:Either<'e,_>, x:Either<'e,'a>, _:Either<'e,'b>) = Applicative.ap f x 

let inline pure' x : ^R = ((^C or ^R) : (static member pure': ^C * ^R -> _) (Applicative, defaultof< ^R>)) x
let inline (<*>) x y : ^R = ((^C or ^a or ^b or ^R) : (static member ap: ^C * ^a * ^b * _-> ^R) (Applicative, x, y,defaultof< ^R>))


type Alternative = Alternative with
    static member empty (Alternative, _:Maybe<'a>) = Nothing
    static member empty (Alternative, _:List<'a> ) = []

    static member append (Alternative, x:Maybe<_>, y) = match x with | None -> y | xs -> xs
    static member append (Alternative, x:List<_> , y) = x ++ y

let inline empty() : ^R = ((^C or ^R) : (static member empty: ^C * ^R -> _) (Alternative, defaultof< ^R>))
let inline (<|>) (x:^a) (y:^a) : ^a = ((^C or ^a) : (static member append: ^C * ^a * ^a -> _) (Alternative, x, y))



let inline (<<|>) f a   = fmap f a
let inline liftA2 f a b = f <<|> a <*> b
let inline (  *>)   x   = x |> liftA2 (const' id)
let inline (<*  )   x   = x |> liftA2 const'
let inline (<**>)   x   = x |> liftA2 (|>)

let inline optional v = Just <<|> v <|> pure' Nothing

type ZipList<'a> = ZipList of 'a seq with
    static member fmap (Functor, ZipList x     ) = fun f -> ZipList (Seq.map f x)
    static member pure' (Applicative, _:ZipList<'a>) = fun (x:'a) -> ZipList (Seq.initInfinite (const' x))
    static member ap    (Applicative, ZipList (f:seq<_>), ZipList x ,_:'R) = ZipList (Seq.zip f x |> Seq.map (fun (f,x) -> f x)) :'R