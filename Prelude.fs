module Prelude

let inline defaultof< ^T> = Unchecked.defaultof< ^T>

let flip f x y = f y x
let const' k _ = k

let (</) = (|>)
let (/>) = flip
let (++) = (@)

type Maybe<'t> = Option<'t>
let  Just x :Maybe<'t> = Some x
let  Nothing:Maybe<'t> = None
let  (|Just|Nothing|) = function Some x -> Just x | _ -> Nothing
let maybe  n f = function | Nothing -> n | Just x -> f x

type Ordering = LT|EQ|GT

let inline compare' x y =
    match compare x y with
    | a when a > 0 -> GT
    | a when a < 0 -> LT
    | _            -> EQ

type Either<'a,'b> = Left of 'a | Right of 'b
let either f g = function Left x -> f x | Right y -> g y


// Num class --------------------------------------------------------------

open System.Numerics
type Integer = bigint

type FromInteger = FromInteger with
    static member        (?<-) (_, _Num:FromInteger, _:sbyte     ) = fun (x:Integer) -> sbyte           x
    static member        (?<-) (_, _Num:FromInteger, _:int16     ) = fun (x:Integer) -> int16           x
    static member        (?<-) (_, _Num:FromInteger, _:int32     ) = fun (x:Integer) -> int             x
    static member        (?<-) (_, _Num:FromInteger, _:int64     ) = fun (x:Integer) -> int64           x
    static member        (?<-) (_, _Num:FromInteger, _:nativeint ) = fun (x:Integer) -> nativeint  (int x)
    static member        (?<-) (_, _Num:FromInteger, _:byte      ) = fun (x:Integer) -> byte            x
    static member        (?<-) (_, _Num:FromInteger, _:uint16    ) = fun (x:Integer) -> uint16          x
    static member        (?<-) (_, _Num:FromInteger, _:uint32    ) = fun (x:Integer) -> uint32          x
    static member        (?<-) (_, _Num:FromInteger, _:uint64    ) = fun (x:Integer) -> uint64          x
    static member        (?<-) (_, _Num:FromInteger, _:unativeint) = fun (x:Integer) -> unativeint (int x)
    static member        (?<-) (_, _Num:FromInteger, _:bigint    ) = fun (x:Integer) ->                 x
    static member        (?<-) (_, _Num:FromInteger, _:float     ) = fun (x:Integer) -> float           x
    static member        (?<-) (_, _Num:FromInteger, _:float32   ) = fun (x:Integer) -> float32         x    
    static member        (?<-) (_, _Num:FromInteger, _:decimal   ) = fun (x:Integer) -> decimal         x
    static member        (?<-) (_, _Num:FromInteger, _:Complex   ) = fun (x:Integer) -> Complex (float  x, 0.0)

let inline fromInteger (x:Integer) :'Num = (() ? (FromInteger) <- defaultof<'Num>) x

type Abs = Abs with
    static member inline (?<-) (_,      Abs, _:^t when ^t: null and ^t: struct) = id
    static member inline (?<-) (_, _Num:Abs, x:^t        ) = abs x
    static member        (?<-) (_, _Num:Abs, x:byte      ) =     x
    static member        (?<-) (_, _Num:Abs, x:uint16    ) =     x
    static member        (?<-) (_, _Num:Abs, x:uint32    ) =     x
    static member        (?<-) (_, _Num:Abs, x:uint64    ) =     x
    static member        (?<-) (_, _Num:Abs, x:unativeint) =     x
    static member        (?<-) (_, _Num:Abs, x:Complex   ) = Complex(x.Magnitude, 0.0)

let inline abs (x:'Num) :'Num = () ? (Abs) <- x

type Signum = Signum with
    static member inline (?<-) (_,      Signum, _:^t when ^t: null and ^t: struct) = id
    static member inline (?<-) (_, _Num:Signum, x:^t        ) = fromInteger (bigint (sign x)) :^t
    static member        (?<-) (_, _Num:Signum, x:byte      ) = if x = 0uy then 0uy else 1uy
    static member        (?<-) (_, _Num:Signum, x:uint16    ) = if x = 0us then 0us else 1us
    static member        (?<-) (_, _Num:Signum, x:uint32    ) = if x = 0u  then 0u  else 1u
    static member        (?<-) (_, _Num:Signum, x:uint64    ) = if x = 0UL then 0UL else 1UL
    static member        (?<-) (_, _Num:Signum, x:unativeint) = if x = 0un then 0un else 1un
    static member        (?<-) (_, _Num:Signum, x:Complex   ) =
        if x.Magnitude = 0.0 then Complex.Zero
        else Complex(x.Real / x.Magnitude, x.Imaginary / x.Magnitude)
   
let inline signum (x:'Num) :'Num = () ? (Signum) <- x

let inline (+) (a:'Num) (b:'Num) :'Num = a + b
let inline (-) (a:'Num) (b:'Num) :'Num = a - b
let inline (*) (a:'Num) (b:'Num) :'Num = a * b

type Negate = Negate with
    static member inline (?<-) (_,      Negate, _:^t when ^t: null and ^t: struct) = id
    static member inline (?<-) (_, _Num:Negate, x:^t        ) = -x
    static member        (?<-) (_, _Num:Negate, x:byte      ) = 0uy - x
    static member        (?<-) (_, _Num:Negate, x:uint16    ) = 0us - x
    static member        (?<-) (_, _Num:Negate, x:uint32    ) = 0u  - x
    static member        (?<-) (_, _Num:Negate, x:uint64    ) = 0UL - x
    static member        (?<-) (_, _Num:Negate, x:unativeint) = 0un - x
   
let inline negate (x:'Num) :'Num = () ? (Negate) <- x
let inline (~-)   (x:'Num) :'Num = () ? (Negate) <- x


// Integral class ---------------------------------------------------------

type ToInteger = ToInteger with
    static member        (?<-) (_, _Integral:ToInteger, x:sbyte     ) = bigint (int x)
    static member        (?<-) (_, _Integral:ToInteger, x:int16     ) = bigint (int x)
    static member        (?<-) (_, _Integral:ToInteger, x:int32     ) = bigint      x
    static member        (?<-) (_, _Integral:ToInteger, x:int64     ) = bigint      x
    static member        (?<-) (_, _Integral:ToInteger, x:nativeint ) = bigint (int x)
    static member        (?<-) (_, _Integral:ToInteger, x:byte      ) = bigint (int x)
    static member        (?<-) (_, _Integral:ToInteger, x:uint16    ) = bigint (int x)
    static member        (?<-) (_, _Integral:ToInteger, x:uint32    ) = bigint      x
    static member        (?<-) (_, _Integral:ToInteger, x:uint64    ) = bigint      x
    static member        (?<-) (_, _Integral:ToInteger, x:unativeint) = bigint (int x)
    static member        (?<-) (_, _Integral:ToInteger, x:bigint    ) =             x

let inline toInteger (x:'Integral) :Integer = () ? (ToInteger) <- x

let inline fromIntegral (x:'Integral) :'Num = (fromInteger << toInteger) x

module NumericLiteralG =
    let inline FromZero() = fromIntegral 0
    let inline FromOne () = fromIntegral 1
    let inline FromInt32  (i:int   ) = fromIntegral i
    let inline FromInt64  (i:int64 ) = fromIntegral i
    let inline FromString (i:string) = fromInteger <| BigInteger.Parse i

let inline whenIntegral a = let _ = if false then toInteger a else 0I in ()

let inline quot (a:'Integral) (b:'Integral) :'Integral = whenIntegral a; a / b
let inline rem  (a:'Integral) (b:'Integral) :'Integral = whenIntegral a; a % b
let inline quotRem a b :'Integral * 'Integral = (quot a b, rem a b)

let inline div (a:'Integral) b :'Integral =
    whenIntegral a
    let (a,b) = if b < 0G then (-a,-b) else (a,b)
    (if a < 0G then (a - b + 1G) else a) / b

let inline mod'   a b :'Integral = whenIntegral a; ((a % b) + b) % b  
let inline divMod a b :'Integral * 'Integral = (div a b, mod' a b)


// Numeric Functions ------------------------------------------------------

let inline gcd x y :'Integral =
    let zero = 0G
    let rec gcd' a = function
        | b when b = zero -> a
        | b -> gcd' b (a </rem/> b)
    match(x,y) with
    | t when t = (zero,zero) -> failwith "Prelude.gcd: gcd 0 0 is undefined"
    | _                      -> gcd' (abs x) (abs y)


// Ratio ------------------------------------------------------------------

module Ratio = 
    type Ratio<'Integral> = private Ratio of 'Integral * 'Integral with
        override this.ToString() =
            let (Ratio(n,d)) = this
            n.ToString() + " % " + d.ToString()

    let inline (%) (a:'Integral) (b:'Integral) :Ratio<'Integral> =
        whenIntegral a
        let zero = 0G
        if b = zero then failwith "Ratio.%: zero denominator"
        let (a,b) = if b < zero then (negate a, negate b) else (a, b)
        let gcd = gcd a b
        Ratio (a </quot/> gcd, b </quot/> gcd)

    let numerator   (Ratio(x,_)) = x
    let denominator (Ratio(_,x)) = x

    type Ratio<'Integral> with
        static member inline (/) (Ratio(a,b),Ratio(c,d)) = (a * d) % (b * c)

        static member inline (+) (Ratio(a,b),Ratio(c,d)) = (a * d + c * b) % (b * d)
        static member inline (-) (Ratio(a,b),Ratio(c,d)) = (a * d - c * b) % (b * d)
        static member inline (*) (Ratio(a,b),Ratio(c,d)) = (a * c) % (b * d)   

open Ratio
type Rational = Ratio<Integer>
let inline (%) (a:'Integral) (b:'Integral) :Ratio<'Integral> = a % b

type Abs         with static member inline (?<-) (_, _Num:Abs        , r:Ratio<_>) = (abs    (numerator r)) % (denominator r)
type Signum      with static member inline (?<-) (_, _Num:Signum     , r:Ratio<_>) = (signum (numerator r)) % 1G
type FromInteger with static member inline (?<-) (_, _Num:FromInteger, _:Ratio<_>) = fun (x:Integer) -> fromInteger x % 1G
type Negate      with static member inline (?<-) (_, _Num:Negate     , r:Ratio<_>) = -(numerator r) % (denominator r)


// Fractional class -------------------------------------------------------

type FromRational = FromRational with
    static member        (?<-) (_, _Fractional:FromRational, _:float   ) = fun (r:Rational) -> float   (numerator r) / float   (denominator r)
    static member        (?<-) (_, _Fractional:FromRational, _:float32 ) = fun (r:Rational) -> float32 (numerator r) / float32 (denominator r)    
    static member        (?<-) (_, _Fractional:FromRational, _:decimal ) = fun (r:Rational) -> decimal (numerator r) / decimal (denominator r)
    static member inline (?<-) (_, _Fractional:FromRational, _:Ratio<_>) = fun (r:Rational) -> fromIntegral  (numerator r) % fromIntegral (denominator r)
    static member        (?<-) (_, _Fractional:FromRational, _:Complex ) = fun (r:Rational) -> Complex(float (numerator r) / float (denominator r), 0.0)

let inline fromRational (x:Rational) :'Fractional = (() ?  (FromRational) <- defaultof<'Fractional>) x

let inline whenFractional a = let _ = if false then fromRational (1I % 1I) else a in ()

let inline (/) (a:'Fractional) (b:'Fractional) :'Fractional = whenFractional a; a / b

let inline recip x :'Fractional = 1G / x


// Exp functions ----------------------------------------------------------

let inline ( **^ ) (x:'Num) (n:'Integral)  = 
    whenIntegral n
    let rec f a b n = if n = 0G then a else f (b * a) b (n - 1G)
    if (n < 0G) then failwith "Negative exponent" else f 1G x n

let inline ( **^^ ) (x:'Fractional) (n:'Integral) = if n >= 0G then x**^n else recip (x**^(negate n))


// RealFrac class ---------------------------------------------------------

type ProperFraction = ProperFraction with
    static member        (?<-) (_, _RealFrac:ProperFraction, x:float   ) = let t = truncate x in (bigint (decimal t), x - t)
    static member        (?<-) (_, _RealFrac:ProperFraction, x:float32 ) = let t = truncate x in (bigint (decimal t), x - t)
    static member        (?<-) (_, _RealFrac:ProperFraction, x:decimal ) = let t = truncate x in (bigint          t , x - t)
    static member inline (?<-) (_, _RealFrac:ProperFraction, r:Ratio<_>) =
        let (a,b) = (numerator r, denominator r)
        let (i,f) = quotRem a b
        (i, f % b)

let inline properFraction (x:'RealFrac) : 'Integral * 'RealFrac =
    let (a, b:'RealFrac) = () ?  (ProperFraction) <- x
    (fromIntegral a, b)

let inline truncate (x:'RealFrac) :'Integral = fst <| properFraction x


// Real class -------------------------------------------------------------

type ToRational = ToRational with
    static member inline (?<-) (_, _Real:ToRational, r:Ratio<_>) = toInteger (numerator r) % toInteger (denominator r) :Rational
    static member inline (?<-) (_, _Real:ToRational, x:^t      ) =
        whenFractional x
        let (i:Integer,d) = properFraction x
        (i % 1I) + (truncate (decimal d * 1000000000000000000000000000M) % 1000000000000000000000000000I) :Rational
    static member inline (?<-) (_, _Real:ToRational, x:^t      ) = (toInteger x) % 1I

let inline toRational (x:'Real) :Rational = () ?  (ToRational) <- x


// Floating class ---------------------------------------------------------

type Pi = Pi with
    static member (?<-) (_, Pi, _:float32) = 3.14159274f
    static member (?<-) (_, Pi, _:float  ) = System.Math.PI
    static member (?<-) (_, Pi, _:Complex) = Complex(System.Math.PI, 0.0)

let inline pi() :'Floating = () ? (Pi) <- defaultof<'Floating>

let inline ( **) a (b:'Floating) :'Floating = a ** b
let inline sqrt    (x:'Floating) :'Floating = sqrt x

let inline asinh x :'Floating = log (x + sqrt (1G+x*x))
let inline acosh x :'Floating = log (x + (x+1G) * sqrt ((x-1G)/(x+1G)))
let inline atanh x :'Floating = (1G/2G) * log ((1G+x) / (1G-x))

let inline logBase x y  :'Floating =  log y / log x


// List functions ---------------------------------------------------------

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
    static member (?<-) (_, _Functor:Fmap, x:Maybe<_>     ) = fun f -> Option.map  f x
    static member (?<-) (_, _Functor:Fmap, x:List<_>      ) = fun f -> List.map    f x  
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
    static member (?<-) (_, _Monad:Return, _:Maybe<'a>    ) = fun (x:'a) -> Just x
    static member (?<-) (_, _Monad:Return, _:List<'a>     ) = fun (x:'a) -> [x]
    static member (?<-) (_, _Monad:Return, _:IO<'a>       ) = fun (x:'a) -> primretIO x
    static member (?<-) (_, _Monad:Return, _: _ -> 'a     ) = fun (x:'a) -> const' x
    static member (?<-) (_, _Monad:Return, _:Either<'e,'a>) = fun (x:'a) -> Right x : Either<'e,'a>

let inline return' x : ^R = (() ? (Return) <- defaultof< ^R> ) x

type Bind = Bind with
    static member (?<-) (x:Maybe<_>    , _Monad:Bind,_:Maybe<'b>    ) = fun (f:_->Maybe<'b>   ) -> Option.bind  f x
    static member (?<-) (x:List<_>     , _Monad:Bind,_:List<'b>     ) = fun (f:_->List<'b>    ) -> List.collect f x
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