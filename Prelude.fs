module Prelude

let const' k _ = k
let maybe n f = function | None -> n | Some x -> f x

type Ordering = LT|EQ|GT

let inline compare' x y =
    match compare x y with
    | a when a > 0 -> GT
    | a when a < 0 -> LT
    | _            -> EQ

type Either<'a,'b> = Left of 'a | Right of 'b
let either f g = function Left x -> f x | Right y -> g y

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
    static member (?<-) (_:unit, _Functor:Fmap, x:option<_>    ) = fun f -> Option.map f x
    static member (?<-) (_:unit, _Functor:Fmap, x:list<_>      ) = fun f -> List.map   f x
    static member (?<-) (_:unit, _Functor:Fmap, x:IO<_>        ) = fun f -> primbindIO x (primretIO << f)
    static member (?<-) (_:unit, _Functor:Fmap, g:_->_         ) = (>>) g
    static member (?<-) (_:unit, _Functor:Fmap, e:Either<'a,'b>) = fun f ->
        match e with
        | (Left x ) -> Left x
        | (Right y) -> Right (f y)

let inline fmap f x = (() ? (Fmap) <- x) f


// Monad class ------------------------------------------------------------

type Return = Return with
    static member (?<-) (_:unit, _Monad:Return, _:'a option   ) = fun (x:'a) -> Some x
    static member (?<-) (_:unit, _Monad:Return, _:'a list     ) = fun (x:'a) -> [x]
    static member (?<-) (_:unit, _Monad:Return, _:'a IO       ) = fun (x:'a) -> primretIO x
    static member (?<-) (_:unit, _Monad:Return, _: _ -> 'a    ) = fun (x:'a) -> const' x
    static member (?<-) (_:unit, _Monad:Return, _:Either<_,'a>) = fun (x:'a) -> Right x

let inline return' x : ^R = (() ? (Return) <- Unchecked.defaultof< ^R> ) x

type Bind = Bind with
    static member (?<-) (x:option<_>  , _Monad:Bind,_:option<'b>) = fun f -> Option.bind f x
    static member (?<-) (x:list<_>    , _Monad:Bind,_:list<'b>  ) = fun f -> let rec bind f = function
                                                                                              | x::xs -> f x @ bind f xs
                                                                                              | []    -> []
                                                                             bind f x
    static member (?<-) (x:IO<_>      , _Monad:Bind,_:IO<'b>) = fun f -> primbindIO x f
    static member (?<-) (f:'e->'a     , _Monad:Bind,_:'e->'b) = fun (k:'a->'e->'b) r -> k (f r) r
    static member (?<-) (x:Either<'e,'a>, _Monad:Bind,_:Either<'e,'b>) = fun (k:_->Either<_,'b>) -> match x with
                                                                                                    | Left  l -> Left l
                                                                                                    | Right r -> k r

let inline (>>=) x f : ^R = (x ? (Bind) <- Unchecked.defaultof< ^R> ) f

// Do notation ------------------------------------------------------------

type DoNotationBuilder() =
    member inline b.Return(x) = return' x
    member inline b.Bind(p,rest) = p >>= rest
    member b.Let(p,rest) = rest p
    member b.ReturnFrom(expr) = expr
let do' = new DoNotationBuilder()