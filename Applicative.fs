module Control.Applicative

open Prelude
open Control.Monad.Base

#nowarn "64"

type Applicative = Applicative with
    static member inline pure'                         x = return' x
    static member pure' (_:Applicative, _:Maybe<'a>    ) = fun (x:'a) -> Applicative.pure' x :Maybe<'a>
    static member pure' (_:Applicative, _:List<'a>     ) = fun (x:'a) -> Applicative.pure' x :List<'a> 
    static member pure' (_:Applicative, _:IO<'a>       ) = fun (x:'a) -> Applicative.pure' x :IO<'a>   
    static member pure' (_:Applicative, _:'r -> 'a     ) = const' : 'a -> ('r -> 'a)
    static member pure' (_:Applicative, _:Either<'e,'a>) = fun (x:'a) -> Applicative.pure' x :Either<'e,'a>

    static member inline ap f = fun x -> ap f x
    static member ap (_:Applicative, f:Maybe<_>    , x:Maybe<'a>    , _:Maybe<'b>    ) = Applicative.ap f x :Maybe<'b>     
    static member ap (_:Applicative, f:List<_>     , x:List<'a>     , _:List<'b>     ) = Applicative.ap f x :List<'b>      
    static member ap (_:Applicative, f:IO<_>       , x:IO<'a>       , _:IO<'b>       ) = Applicative.ap f x :IO<'b>        
    static member ap (_:Applicative, f:'r -> _     , g: _ -> 'a     , _: 'r -> 'b    ) = fun x -> f x (g x) :'b
    static member ap (_:Applicative, f:Either<'e,_>, x:Either<'e,'a>, _:Either<'e,'b>) = Applicative.ap f x :Either<'e,'b> 

let inline pure' x   : ^R = ((^C       or       ^R) : (static member pure': ^C        * ^R -> _) (Applicative,       defaultof< ^R>)) x
let inline (<*>) x y : ^R = ((^C or ^a or ^b or ^R) : (static member ap: ^C * ^a * ^b * ^R -> _) (Applicative, x, y, defaultof< ^R>))


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

type ZipList<'s> = ZipList of 's seq with
    static member fmap (Functor, ZipList x     ) = fun (f:'a->'b) -> ZipList (Seq.map f x)
    static member pure' (Applicative, _:ZipList<'a>) = fun (x:'a) -> ZipList (Seq.initInfinite (const' x))
    static member ap    (Applicative, ZipList (f:seq<'a->'b>), ZipList x ,_:'R) = ZipList (Seq.zip f x |> Seq.map (fun (f,x) -> f x)) :'R