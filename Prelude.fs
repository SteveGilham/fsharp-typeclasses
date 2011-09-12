module Prelude

let const' k _ = k

// IO ---------------------------------------------------------------------

type IO<'a> = IO of (unit->'a) with static member Invoke(IO(f)) : 'a = f()
let primretIO  f    = IO(fun () -> f)
let primbindIO io f = IO(fun () -> IO.Invoke (f (IO.Invoke io )))

let getLine    = IO(fun() -> System.Console.ReadLine())
let putStrLn x = IO(fun() -> printfn "%s" x)


// Functor class ----------------------------------------------------------

type Fmap() =
    static member (?) (x:option<_>,cs:Fmap)  = fun f -> Option.map  f x
    static member (?) (x:list<_>  ,cs:Fmap)  = fun f -> List.map    f x
    static member (?) (x:IO<_>    ,cs:Fmap)  = fun f -> primbindIO x (primretIO << f)
    static member (?) (g:_->_     ,cs:Fmap)  = fun f x -> f (g x)

let inline fmap f x = (x ? (Fmap()) ) f


// Monad class ------------------------------------------------------------

type Return() =
    static member (?<-) (notUsed:Return, cs:Return, t:'a option) = fun (x:'a) -> Some x
    static member (?<-) (notUsed:Return, cs:Return, t:'a list)   = fun (x:'a) -> [x]
    static member (?<-) (notUsed:Return, cs:Return, t:'a IO )    = fun (x:'a) -> primretIO x
    static member (?<-) (notUsed:Return, cs:Return, t: _ -> 'a)  = fun (x:'a) -> const' x

let inline return' x : 'R = ((Return()) ? ( Return()) <- Unchecked.defaultof< 'R> ) x

type Bind() =
    static member (?) (x:option<_>, cs:Bind) =
        fun k ->
            match x with
            | Some v -> k v 
            | None   -> None

    static member (?) (x:list<_>  , cs:Bind) =
        fun f ->
            let rec bindForLists lst f =
                match lst with
                | x::xs -> f x @ (bindForLists xs f)
                | [] -> [] 
            bindForLists x f

    static member (?) (x:IO<_>, cs:Bind)   = fun f -> primbindIO x f

let inline (>>=) x = x ? (Bind())


// Do notation ------------------------------------------------------------

type DoNotationBuilder() =
    member inline b.Return(x) = return' x
    member inline b.Bind(p,rest) = p >>= rest
    member b.Let(p,rest) = rest p

let do' = new DoNotationBuilder()