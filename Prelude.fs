module Prelude

let const' k _ = k

type Ordering = LT|EQ|GT

let inline compare' x y =
    match compare x y with
    | a when a > 0 -> GT
    | a when a < 0 -> LT
    | _ -> EQ

type Either<'a,'b> = Left of 'a | Right of 'b
let either f g = function Left x -> f x | Right y -> g y

// IO ---------------------------------------------------------------------

type IO<'a> = IO of (unit->'a) with static member Invoke(IO(f)) : 'a = f()
let primretIO  f    = IO(fun () -> f)
let primbindIO io f = IO(fun () -> IO.Invoke (f (IO.Invoke io )))

let getLine    = IO(fun() -> System.Console.ReadLine())
let putStrLn x = IO(fun() -> printfn "%s" x)


// Functor class ----------------------------------------------------------

type Fmap = Fmap with
    static member (?) (x:option<_>    , _Functor:Fmap) = fun f -> Option.map f x
    static member (?) (x:list<_>      , _Functor:Fmap) = fun f -> List.map   f x
    static member (?) (x:IO<_>        , _Functor:Fmap) = fun f -> primbindIO x (primretIO << f)
    static member (?) (g:_->_         , _Functor:Fmap) = (>>) g
    static member (?) (e:Either<'a,'b>, _Functor:Fmap) = 
        fun f ->
            match e with
            | (Left x ) -> Left x
            | (Right y) -> Right (f y)

let inline fmap f x = (x ? (Fmap) ) f


// Monad class ------------------------------------------------------------

type Return = Return with
    static member (?<-) (_:Return, _Monad:Return, t:'a option) = fun (x:'a) -> Some x
    static member (?<-) (_:Return, _Monad:Return, t:'a list)   = fun (x:'a) -> [x]
    static member (?<-) (_:Return, _Monad:Return, t:'a IO )    = fun (x:'a) -> primretIO x
    static member (?<-) (_:Return, _Monad:Return, t: _ -> 'a)  = fun (x:'a) -> const' x
    static member (?<-) (_:Return, _Monad:Return, t:Either<_,'a>) = fun (x:'a) -> Right x

let inline return' x : ^R = (Return ? (Return) <- Unchecked.defaultof< ^R> ) x

type Bind = Bind with
    static member (?<-) (x:option<_>  , _Monad:Bind,t:option<'b>) = fun f -> Option.bind f x
    static member (?<-) (x:list<_>    , _Monad:Bind,t:list<'b>  ) = fun f ->
        let rec bindForList lst f =
            match lst with
            | x::xs -> f x @ (bindForList xs f)
            | [] -> [] 
        bindForList x f
    static member (?<-) (x:IO<_>      , _Monad:Bind,t:IO<'b>) = fun f -> primbindIO x f
    static member (?<-) (f:'e->'a     , _Monad:Bind,t:'e->'b) = fun (k:'a->'e->'b) r -> k (f r) r
    static member (?<-) (x:Either<'e,'a>, _Monad:Bind,t:Either<'e,'b>) = fun (k:_->Either<_,'b>) -> match x with
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