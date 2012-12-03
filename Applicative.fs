module Control.Applicative

open Prelude
open Control.Monad.Base

module Applicative =
    let inline pure' x = return' x
    let inline ap  f x = ap f x

    type Pure = Pure with
        static member instance (Pure, _:option<'a>   ) = fun (x:'a) -> pure' x :option<'a>
        static member instance (Pure, _:List<'a>     ) = fun (x:'a) -> pure' x :List<'a>
        static member instance (Pure, _:'r -> 'a     ) = const':'a  -> 'r -> _
        static member instance (Pure, _:Async<'a>    ) = fun (x:'a) -> pure' x :Async<'a>
        static member instance (Pure, _:Choice<'a,'e>) = fun (x:'a) -> pure' x :Choice<_,'e>

    type Ap = Ap with
        static member instance (Ap, f:option<_>   , x:option<'a>   , _:option<'b>   ) = fun () -> ap f x :option<'b>
        static member instance (Ap, f:List<_>     , x:List<'a>     , _:List<'b>     ) = fun () -> ap f x :List<'b>
        static member instance (Ap, f:'r -> _     , g: _ -> 'a     , _: 'r -> 'b    ) = fun () -> fun x -> f x (g x) :'b
        static member instance (Ap, f:Async<_>    , x:Async<'a>    , _:Async<'b>    ) = fun () -> ap f x :Async<'b>
        static member instance (Ap, f:Choice<_,'e>, x:Choice<'a,'e>, _:Choice<'b,'e>) = fun () -> ap f x :Choice<'b,'e>

let inline pure' x   = Inline.instance Applicative.Pure x
let inline (<*>) x y = Inline.instance (Applicative.Ap, x, y) ()

module Alternative =
    type Empty = Empty with
        static member instance (_Alternative:Empty, _:option<'a>) = fun () -> None
        static member instance (_Alternative:Empty, _:List<'a>  ) = fun () -> []

    type Append = Append with   
        static member instance (_Alternative:Append, x:option<_>, _) = fun y -> match x with | None -> y | xs -> xs
        static member instance (_Alternative:Append, x:List<_>  , _) = fun y -> x @ y

let inline empty() = Inline.instance Alternative.Empty ()
let inline (<|>) (x:'a) (y:'a) :'a = Inline.instance (Alternative.Append, x) y


let inline (<<|>) f a   = fmap f a
let inline liftA2 f a b = f <<|> a <*> b
let inline (  *>)   x   = x |> liftA2 (const' id)
let inline (<*  )   x   = x |> liftA2 const'
let inline (<**>)   x   = x |> liftA2 (|>)

let inline optional v = Some <<|> v <|> pure' None

type ZipList<'s> = ZipList of 's seq with
    static member instance (Functor    .Fmap,   ZipList x  , _) = fun (f:'a->'b) -> ZipList (Seq.map f x)
    static member instance (Applicative.Pure, _:ZipList<'a>   ) = fun (x:'a)     -> ZipList (Seq.initInfinite (const' x))
    static member instance (Applicative.Ap  ,   ZipList (f:seq<'a->'b>), ZipList x ,_:ZipList<'b>) = fun () ->
        ZipList (Seq.zip f x |> Seq.map (fun (f,x) -> f x)) :ZipList<'b>