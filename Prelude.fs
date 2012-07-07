module Prelude

let inline defaultof< ^T> = Unchecked.defaultof< ^T>

let flip f x y = f y x
let const' k _ = k
let maybe  n f = function | None -> n | Some x -> f x

type Ordering = LT|EQ|GT

let inline compare' x y =
    match compare x y with
    | a when a > 0 -> GT
    | a when a < 0 -> LT
    | _            -> EQ

type Either<'a,'b> = Left of 'a | Right of 'b
let either f g = function Left x -> f x | Right y -> g y


// List -------------------------------------------------------------------

let map, replicate, filter, head, tail = List.map, List.replicate, List.filter, List.head, List.tail
let last x = List.length x - 1 |> fun e -> x.[e]
let init x = List.init (List.length x-1) (fun e -> x.[e])
let length,reverse = List.length,List.rev

let foldr f z x = List.foldBack f x z
let rec foldr1 f = function [x] -> x | (x::xs) -> f x (foldr1 f xs) | [] -> failwith "EmptyList foldr1"
let foldl = List.fold
let rec foldl1 f = function (x::xs) -> foldl f x xs | [] -> failwith "EmptyList foldl1"


// IO ---------------------------------------------------------------------

type IO<'a> = IO of (unit->'a)
let runIO  (IO f)   = f()
let primretIO  f    = IO(fun () -> f)
let primbindIO io f = IO(fun () -> runIO (f (runIO io )))

let getLine    = IO(fun() -> System.Console.ReadLine())
let putStrLn x = IO(fun() -> printfn "%s" x)
let print    x = IO(fun() -> printfn "%A" x)


// Functor class ----------------------------------------------------------

type Fmap = Fmap with
    static member (?<-) (_, _Functor:Fmap, x:option<_>    ) = fun f -> Option.map  f x
    static member (?<-) (_, _Functor:Fmap, x:list<_>      ) = fun f -> List.map    f x    
    static member (?<-) (_, _Functor:Fmap, x:IO<_>        ) = fun f -> primbindIO  x (primretIO << f)
    static member (?<-) (_, _Functor:Fmap, g:_->_         ) = (>>) g
    static member (?<-) (_, _Functor:Fmap, e:Either<'a,'b>) = fun f ->
        match e with
        | (Left x ) -> Left x
        | (Right y) -> Right (f y)
    static member (?<-) (_, _Functor:Fmap, x:array<_>     ) = fun f -> Array.map   f x
    static member (?<-) (_, _Functor:Fmap, x:_ [,]        ) = fun f -> Array2D.map f x
    static member (?<-) (_, _Functor:Fmap, x:_ [,,]       ) = fun f -> Array3D.map f x
    static member (?<-) (_, _Functor:Fmap, x:_ [,,,]      ) = fun f -> 
        Array4D.init (x.GetLength 0) (x.GetLength 1) (x.GetLength 2) (x.GetLength 3) (fun a b c d -> f x.[a,b,c,d])

let inline fmap f x = (() ? (Fmap) <- x) f


// Monad class ------------------------------------------------------------

type Return = Return with
    static member (?<-) (_, _Monad:Return, _:'a option    ) = fun (x:'a) -> Some x
    static member (?<-) (_, _Monad:Return, _:'a list      ) = fun (x:'a) -> [x]
    static member (?<-) (_, _Monad:Return, _:'a IO        ) = fun (x:'a) -> primretIO x
    static member (?<-) (_, _Monad:Return, _: _ -> 'a     ) = fun (x:'a) -> const' x
    static member (?<-) (_, _Monad:Return, _:Either<'e,'a>) = fun (x:'a) -> Right x : Either<'e,'a>

let inline return' x : ^R = (() ? (Return) <- defaultof< ^R> ) x

type Bind = Bind with
    static member (?<-) (x:option<_>   , _Monad:Bind,_:option<'b>   ) = fun (f:_->option<'b>  ) -> Option.bind  f x
    static member (?<-) (x:list<_>     , _Monad:Bind,_:list<'b>     ) = fun (f:_->list<'b>    ) -> List.collect f x
    static member (?<-) (x:IO<_>       , _Monad:Bind,_:IO<'b>       ) = fun (f:_->IO<'b>      ) -> primbindIO x f
    static member (?<-) (f:'e->'a      , _Monad:Bind,_:'e->'b       ) = fun (k:_->_->'b) r      -> k (f r) r
    static member (?<-) (x:Either<'e,_>, _Monad:Bind,_:Either<'e,'b>) = fun (k:_->Either<_,'b>) -> match x with
                                                                                                   | Left  l -> Left l
                                                                                                   | Right r -> k r

let inline (>>=) x f : ^R = (x ? (Bind) <- defaultof< ^R> ) f
let inline (=<<) f x : ^R = (x ? (Bind) <- defaultof< ^R> ) f

// Do notation ------------------------------------------------------------

type DoNotationBuilder() =
    member inline b.Return(x)    = return' x
    member inline b.Bind(p,rest) = p >>= rest
    member        b.Let (p,rest) = rest p
    member    b.ReturnFrom(expr) = expr
let do' = new DoNotationBuilder()