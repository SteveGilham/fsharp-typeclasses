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
    static member (?) (x:option<_>,cs:Fmap)  = fun f -> Option.map  f x
    static member (?) (x:list<_>  ,cs:Fmap)  = fun f -> List.map    f x
    static member (?) (x:IO<_>    ,cs:Fmap)  = fun f -> primbindIO x (primretIO << f)
    static member (?) (g:_->_     ,cs:Fmap)  = (>>) g
    static member (?) (e:Either<'a,'b>,cs:Fmap) = 
        fun f ->
            match e with
            | (Left x ) -> Left x
            | (Right y) -> Right (f y)

let inline fmap f x = (x ? (Fmap) ) f


// Monad class ------------------------------------------------------------

type Return = Return with
    static member (?<-) (_:Return, cs:Return, t:'a option) = fun (x:'a) -> Some x
    static member (?<-) (_:Return, cs:Return, t:'a list)   = fun (x:'a) -> [x]
    static member (?<-) (_:Return, cs:Return, t:'a IO )    = fun (x:'a) -> primretIO x
    static member (?<-) (_:Return, cs:Return, t: _ -> 'a)  = fun (x:'a) -> const' x
    static member (?<-) (_:Return, cs:Return, t:Either<_,'a>) = fun (x:'a) -> Right x

let inline return' x : ^R = (Return ? (Return) <- Unchecked.defaultof< ^R> ) x

type Bind = Bind with
    static member (?) (x:option<_>, cs:Bind) = fun k -> match x with
                                                        | Some v -> k v 
                                                        | None   -> None
    static member (?) (x:list<_>  , cs:Bind) =
        fun f ->
            let rec bindForLists lst f =
                match lst with
                | x::xs -> f x @ (bindForLists xs f)
                | [] -> [] 
            bindForLists x f
    static member (?) (x:IO<_>, cs:Bind) = fun f -> primbindIO x f
    static member (?) (f:_->_ , cs:Bind) = fun k r -> k (f r) r
    static member (?) (x:Either<_,_>, cs:Bind) = fun k -> match x with
                                                          | Left  l -> Left l
                                                          | Right r -> k r

let inline (>>=) x = x ? (Bind)


// Do notation ------------------------------------------------------------

type DoNotationBuilder() =
    member inline b.Return(x) = return' x
    member inline b.Bind(p,rest) = p >>= rest
    member b.Let(p,rest) = rest p

let do' = new DoNotationBuilder()