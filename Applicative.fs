namespace InlineAbstractions.TypeClasses

open InlineAbstractions.Prelude
open InlineAbstractions.TypeClasses.Monad

module Applicative =
    type DefaultImpl = 
        static member inline Pure x = return' x
        static member inline Ap f x = f >>= fun x1 -> x >>= fun x2 -> return'(x1 x2)

    type Pure = Pure with        
        //member inline this.defaultImpl x = return' x
        static member instance (Pure, _:option<'a>   ) = fun (x:'a) -> DefaultImpl.Pure x :option<'a>
        static member instance (Pure, _:List<'a>     ) = fun (x:'a) -> DefaultImpl.Pure x :List<'a>
        static member instance (Pure, _:'r -> 'a     ) = const':'a  -> 'r -> _
        static member instance (Pure, _:Async<'a>    ) = fun (x:'a) -> DefaultImpl.Pure x :Async<'a>
        static member instance (Pure, _:Choice<'a,'e>) = fun (x:'a) -> DefaultImpl.Pure x :Choice<_,'e>

    // type DefaultImpl with static member inline Ap f x = f >>= fun x1 -> x >>= fun x2 -> return'(x1 x2)
    type Ap = Ap with
        //member inline this.defaultImpl  f x = f >>= fun x1 -> x >>= fun x2 -> return'(x1 x2)
        static member instance (Ap, f:option<_>   , x:option<'a>   , _:option<'b>   ) = fun () -> DefaultImpl.Ap f x :option<'b>
        static member instance (Ap, f:List<_>     , x:List<'a>     , _:List<'b>     ) = fun () -> DefaultImpl.Ap f x :List<'b>
        static member instance (Ap, f:'r -> _     , g: _ -> 'a     , _: 'r -> 'b    ) = fun () -> fun x -> f x (g x) :'b
        static member instance (Ap, f:Async<_>    , x:Async<'a>    , _:Async<'b>    ) = fun () -> DefaultImpl.Ap f x :Async<'b>
        static member instance (Ap, f:Choice<_,'e>, x:Choice<'a,'e>, _:Choice<'b,'e>) = fun () ->
            match (f,x) with
            | (Choice1Of2 a, Choice1Of2 b) -> Choice1Of2 (a b)
            | (Choice2Of2 a, _)            -> Choice2Of2 a
            | (_, Choice2Of2 b)            -> Choice2Of2 b :Choice<'b,'e>

    let inline internal pure' x   = Inline.instance Pure x
    let inline internal (<*>) x y = Inline.instance (Ap, x, y) ()

module Alternative =
    type Empty = Empty with
        static member instance (_Alternative:Empty, _:option<'a>) = fun () -> None
        static member instance (_Alternative:Empty, _:List<'a>  ) = fun () -> []

    type Append = Append with   
        static member instance (_Alternative:Append, x:option<_>, _) = fun y -> match x with | None -> y | xs -> xs
        static member instance (_Alternative:Append, x:List<_>  , _) = fun y -> x @ y

    let inline internal empty() = Inline.instance Empty ()
    let inline internal (<|>) (x:'a) (y:'a) :'a = Inline.instance (Append, x) y

    open InlineAbstractions.TypeClasses.Functor
    let inline internal (<<|>) f a   = fmap f a
    let inline internal liftA2 f a b = f <<|> a <*> b
    let inline internal (  *>)   x   = x |> liftA2 (const' id)
    let inline internal (<*  )   x   = x |> liftA2 const'
    let inline internal (<**>)   x   = x |> liftA2 (|>)

    // open InlineAbstractions.TypeClasses.Applicative
    // let inline internal optional v = Some <<|> v <|> pure' None

(*
type ZipList<'s> = ZipList of 's seq with
    static member instance (InlineAbstractions.TypeClasses.Functor.Fmap,   ZipList x  , _) = fun (f:'a->'b) -> ZipList (Seq.map f x)
    static member instance (Applicative.Pure, _:ZipList<'a>   ) = fun (x:'a)     -> 
        let x = Applicative.DefaultImpl.Pure 7
        ZipList (Seq.initInfinite (const' x))
    static member instance (Applicative.Ap  ,   ZipList (f:seq<'a->'b>), ZipList x ,_:ZipList<'b>) = fun () ->
        let x = App
        ZipList (Seq.zip f x |> Seq.map (fun (f,x) -> f x)) :ZipList<'b> *)