module Prelude

let flip f x y = f y x
let const' k _ = k

let (</) = (|>)
let (/>) = flip
let (++) = (@)
let (==) = (=)
let (=/) x y = not (x = y)
let choice f g = function Choice2Of2 x -> f x | Choice1Of2 y -> g y

type DeReference = DeReference with
    static member instance (DeReference, a:'a ref     , _) = fun () -> !a
    static member instance (DeReference, a:string     , _) = fun () -> a.ToCharArray() |> Array.toList
    static member instance (DeReference, a:DeReference, _) = fun () -> DeReference

let inline (!) a = Inline.instance (DeReference, a) ()

let maybe  n f = function | None -> n | Some x -> f x


// Num class --------------------------------------------------------------

open System.Numerics
type Integer = bigint

module Num =
    type FromInteger = FromInteger with
        static member        instance (FromInteger, _:sbyte     ) = fun (x:Integer) -> sbyte           x
        static member        instance (FromInteger, _:int16     ) = fun (x:Integer) -> int16           x
        static member        instance (FromInteger, _:int32     ) = fun (x:Integer) -> int             x
        static member        instance (FromInteger, _:int64     ) = fun (x:Integer) -> int64           x
        static member        instance (FromInteger, _:nativeint ) = fun (x:Integer) -> nativeint  (int x)
        static member        instance (FromInteger, _:byte      ) = fun (x:Integer) -> byte            x
        static member        instance (FromInteger, _:uint16    ) = fun (x:Integer) -> uint16          x
        static member        instance (FromInteger, _:uint32    ) = fun (x:Integer) -> uint32          x
        static member        instance (FromInteger, _:uint64    ) = fun (x:Integer) -> uint64          x
        static member        instance (FromInteger, _:unativeint) = fun (x:Integer) -> unativeint (int x)
        static member        instance (FromInteger, _:bigint    ) = fun (x:Integer) ->                 x
        static member        instance (FromInteger, _:float     ) = fun (x:Integer) -> float           x
        static member        instance (FromInteger, _:float32   ) = fun (x:Integer) -> float32         x    
        static member        instance (FromInteger, _:decimal   ) = fun (x:Integer) -> decimal         x
        static member        instance (FromInteger, _:Complex   ) = fun (x:Integer) -> Complex (float  x, 0.0)

    let inline fromInteger (x:Integer) :'Num = Inline.instance FromInteger x
    

    type Abs = Abs with
        static member inline instance (Abs, _:^t when ^t: null and ^t: struct, _) = fun () -> id
        static member inline instance (Abs, x:'t        , _) = fun () -> abs x
        static member        instance (Abs, x:byte      , _) = fun () ->     x
        static member        instance (Abs, x:uint16    , _) = fun () ->     x
        static member        instance (Abs, x:uint32    , _) = fun () ->     x
        static member        instance (Abs, x:uint64    , _) = fun () ->     x
        static member        instance (Abs, x:unativeint, _) = fun () ->     x
        static member        instance (Abs, x:Complex   , _) = fun () -> Complex(x.Magnitude, 0.0)

    

    type Signum = Signum with
        static member inline instance (Signum, _:^t when ^t: null and ^t: struct, _) = fun () -> id
        static member inline instance (Signum, x:'t        , _) = fun () -> fromInteger (bigint (sign x)) :'t
        static member        instance (Signum, x:byte      , _) = fun () -> if x == 0uy then 0uy else 1uy
        static member        instance (Signum, x:uint16    , _) = fun () -> if x == 0us then 0us else 1us
        static member        instance (Signum, x:uint32    , _) = fun () -> if x == 0u  then 0u  else 1u
        static member        instance (Signum, x:uint64    , _) = fun () -> if x == 0UL then 0UL else 1UL
        static member        instance (Signum, x:unativeint, _) = fun () -> if x == 0un then 0un else 1un
        static member        instance (Signum, x:Complex   , _) = fun () -> 
            if x.Magnitude == 0.0 then Complex.Zero
            else Complex(x.Real / x.Magnitude, x.Imaginary / x.Magnitude)
   


    type Negate = Negate with
        static member inline instance (Negate, _:^t when ^t: null and ^t: struct, _) = fun () -> id
        static member inline instance (Negate, x:'t        , _) = fun () -> -x
        static member        instance (Negate, x:byte      , _) = fun () -> 0uy - x
        static member        instance (Negate, x:uint16    , _) = fun () -> 0us - x
        static member        instance (Negate, x:uint32    , _) = fun () -> 0u  - x
        static member        instance (Negate, x:uint64    , _) = fun () -> 0UL - x
        static member        instance (Negate, x:unativeint, _) = fun () -> 0un - x

let inline fromInteger (x:Integer) :'Num = Num.fromInteger x
let inline abs (x:'Num) :'Num = Inline.instance (Num.Abs, x) ()
let inline signum (x:'Num) :'Num = Inline.instance (Num.Signum, x) ()

let inline (+) (a:'Num) (b:'Num) :'Num = a + b
let inline (-) (a:'Num) (b:'Num) :'Num = a - b
let inline (*) (a:'Num) (b:'Num) :'Num = a * b
   
let inline negate (x:'Num) :'Num = Inline.instance (Num.Negate, x) ()
let inline (~-)   (x:'Num) :'Num = Inline.instance (Num.Negate, x) ()



// Integral class ---------------------------------------------------------
module Integral =
    type ToInteger = ToInteger with
        static member        instance (ToInteger, x:sbyte     , _) = fun () -> bigint (int x)
        static member        instance (ToInteger, x:int16     , _) = fun () -> bigint (int x)
        static member        instance (ToInteger, x:int32     , _) = fun () -> bigint      x
        static member        instance (ToInteger, x:int64     , _) = fun () -> bigint      x
        static member        instance (ToInteger, x:nativeint , _) = fun () -> bigint (int x)
        static member        instance (ToInteger, x:byte      , _) = fun () -> bigint (int x)
        static member        instance (ToInteger, x:uint16    , _) = fun () -> bigint (int x)
        static member        instance (ToInteger, x:uint32    , _) = fun () -> bigint      x
        static member        instance (ToInteger, x:uint64    , _) = fun () -> bigint      x
        static member        instance (ToInteger, x:unativeint, _) = fun () -> bigint (int x)
        static member        instance (ToInteger, x:bigint    , _) = fun () ->             x

let inline toInteger (x:'Integral) :Integer = Inline.instance (Integral.ToInteger, x) ()

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
    type Ratio<'Integral> = Ratio of 'Integral * 'Integral with
        override this.ToString() =
            let (Ratio(n,d)) = this
            n.ToString() + " % " + d.ToString()

    let inline (%) (a:'Integral) (b:'Integral) :Ratio<'Integral> =
        whenIntegral a
        let zero = 0G
        if b == zero then failwith "Ratio.%: zero denominator"
        let (a,b) = if b < zero then (negate a, negate b) else (a, b)
        let gcd = gcd a b
        Ratio (a </quot/> gcd, b </quot/> gcd)

    let inline Ratio (x,y) = x % y
    let numerator   (Ratio(x,_)) = x
    let denominator (Ratio(_,x)) = x

    type Ratio<'Integral> with
        static member inline (/) (Ratio(a,b), Ratio(c,d)) = (a * d) % (b * c)
                                              
        static member inline (+) (Ratio(a,b), Ratio(c,d)) = (a * d + c * b) % (b * d)
        static member inline (-) (Ratio(a,b), Ratio(c,d)) = (a * d - c * b) % (b * d)
        static member inline (*) (Ratio(a,b), Ratio(c,d)) = (a * c) % (b * d)   

    type Ratio<'RA> with static member inline instance (Num.Abs        , r:Ratio<_>, _) = fun () -> (abs    (numerator r)) % (denominator r)
    type Ratio<'RA> with static member inline instance (Num.Signum     , r:Ratio<_>, _) = fun () -> (signum (numerator r)) % 1G
    type Ratio<'RA> with static member inline instance (dm:Num.FromInteger, _:Ratio<_>) = fun (x:Integer) -> Num.fromInteger x % 1G
    type Ratio<'RA> with static member inline instance (Num.Negate     , r:Ratio<_>, _) = fun () -> -(numerator r) % (denominator r)

type Rational = Ratio.Ratio<Integer>
open Ratio
let inline (%) (a:'Integral) (b:'Integral) :Ratio<'Integral> = a % b



// Fractional class -------------------------------------------------------
module Fractional =
    type FromRational = FromRational with
        static member        instance (FromRational, _:float   ) = fun (r:Rational) -> float   (numerator r) / float   (denominator r)
        static member        instance (FromRational, _:float32 ) = fun (r:Rational) -> float32 (numerator r) / float32 (denominator r)    
        static member        instance (FromRational, _:decimal ) = fun (r:Rational) -> decimal (numerator r) / decimal (denominator r)
        static member inline instance (FromRational, _:Ratio<_>) = fun (r:Rational) -> fromIntegral  (numerator r) % fromIntegral (denominator r)
        static member        instance (FromRational, _:Complex ) = fun (r:Rational) -> Complex(float (numerator r) / float (denominator r), 0.0)

let inline fromRational (x:Rational) :'Fractional = Inline.instance Fractional.FromRational x

let inline whenFractional a = let _ = if false then fromRational (1I % 1I) else a in ()

let inline (/) (a:'Fractional) (b:'Fractional) :'Fractional = whenFractional a; a / b

let inline recip x :'Fractional = 1G / x


// Exp functions ----------------------------------------------------------

let inline ( **^ ) (x:'Num) (n:'Integral)  = 
    whenIntegral n
    let rec f a b n = if n == 0G then a else f (b * a) b (n - 1G)
    if (n < 0G) then failwith "Negative exponent" else f 1G x n

let inline ( **^^ ) (x:'Fractional) (n:'Integral) = if n >= 0G then x**^n else recip (x**^(negate n))


// RealFrac class ---------------------------------------------------------
module RealFrac =
    type ProperFraction = ProperFraction with
        static member        instance (ProperFraction, x:float   , _) = fun () -> let t = truncate x in (bigint (decimal t), x - t)
        static member        instance (ProperFraction, x:float32 , _) = fun () -> let t = truncate x in (bigint (decimal t), x - t)
        static member        instance (ProperFraction, x:decimal , _) = fun () -> let t = truncate x in (bigint          t , x - t)
        static member inline instance (ProperFraction, r:Ratio<_>, _) = fun () -> 
            let (a,b) = (numerator r, denominator r)
            let (i,f) = quotRem a b
            (i, f % b)

let inline properFraction (x:'RealFrac) : 'Integral * 'RealFrac =
    let (a, b:'RealFrac) = Inline.instance (RealFrac.ProperFraction, x) ()
    (fromIntegral a, b)

let inline truncate (x:'RealFrac) :'Integral = fst <| properFraction x


// Real class -------------------------------------------------------------

module Real =
    type ToRational = ToRational with
        static member inline instance (ToRational, r:Ratio<_>, _) = fun () -> toInteger (numerator r) % toInteger (denominator r) :Rational
        static member inline instance (ToRational, x:'t      , _) = fun () -> 
            whenFractional x
            let (i:Integer,d) = properFraction x
            (i % 1I) + (truncate (decimal d * 1000000000000000000000000000M) % 1000000000000000000000000000I) :Rational
        static member inline instance (ToRational, x:'t      , _) = fun () -> (toInteger x) % 1I

let inline toRational (x:'Real) :Rational = Inline.instance (Real.ToRational, x) ()


// Floating class ---------------------------------------------------------

module Floating =
    type Pi = Pi with
        static member instance (Pi, _:float32) = fun () -> 3.14159274f
        static member instance (Pi, _:float  ) = fun () -> System.Math.PI
        static member instance (Pi, _:Complex) = fun () -> Complex(System.Math.PI, 0.0)

let inline pi() :'Floating = Inline.instance Floating.Pi ()

let inline ( **) a (b:'Floating) :'Floating = a ** b
let inline sqrt    (x:'Floating) :'Floating = sqrt x

let inline asinh x :'Floating = log (x + sqrt (1G+x*x))
let inline acosh x :'Floating = log (x + (x+1G) * sqrt ((x-1G)/(x+1G)))
let inline atanh x :'Floating = (1G/2G) * log ((1G+x) / (1G-x))

let inline logBase x y  :'Floating =  log y / log x


// IO ---------------------------------------------------------------------

let runIO = Async.RunSynchronously
let primretIO   = async.Return
let primbindIO io f = async.Bind(io,f)

let getLine    = async.Delay(fun () -> async.Return (System.Console.ReadLine()))
let putStrLn x = async.Delay(fun () -> async.Return (printfn "%s" x))
let print    x = async.Delay(fun () -> async.Return (printfn "%A" x))


// List functions ---------------------------------------------------------

let map, replicate, filter, head, tail = List.map, List.replicate, List.filter, List.head, List.tail
let last x = List.length x - 1 |> fun e -> x.[e]
let init x = List.init (List.length x-1) (fun e -> x.[e])
let length,reverse = List.length,List.rev

let foldr f z x = List.foldBack f x z
let rec foldr1 f = function [x] -> x | (x::xs) -> f x (foldr1 f xs) | [] -> failwith "EmptyList foldr1"
let foldl = List.fold
let rec foldl1 f = function (x::xs) -> foldl f x xs | [] -> failwith "EmptyList foldl1"



// Functor class ----------------------------------------------------------

open System
let (|Null|Value|) (x: _ Nullable) = if x.HasValue then Value x.Value else Null

module Functor =
    type Fmap = Fmap with
        static member instance (Fmap, x:option<_>    , _) = fun f -> Option.map  f x
        static member instance (Fmap, x:List<_>      , _:List<'b>) = fun f -> List.map    f x :List<'b>
        static member instance (Fmap, g:_->_         , _) = (>>) g
        static member instance (Fmap, x:array<_>     , _) = fun f -> Array.map   f x
        static member instance (Fmap, x:_ [,]        , _) = fun f -> Array2D.map f x
        static member instance (Fmap, x:_ [,,]       , _) = fun f -> Array3D.map f x
        static member instance (Fmap, x:_ [,,,]      , _) = fun f ->
            Array4D.init (x.GetLength 0) (x.GetLength 1) (x.GetLength 2) (x.GetLength 3) (fun a b c d -> f x.[a,b,c,d])
        static member instance (Fmap, x:Async<_>   , _) = fun f -> async.Bind(x,f >> async.Return)
        static member instance (Fmap, x:Nullable<_>, _) = fun f -> match x with
                                                                            | Null -> Nullable()
                                                                            | Value v -> Nullable(f v)
        static member instance (Fmap, x:Choice<_,_>, _) = fun f -> match x with
                                                                            | Choice2Of2 x -> Choice2Of2(f x)
                                                                            | Choice1Of2 x -> Choice1Of2 x

let inline fmap f x = Inline.instance (Functor.Fmap, x) f


// Monad class ------------------------------------------------------------
module Monad =
    type Return = Return with
        static member instance (Return, _:option<'a>   ) = fun x -> Some x      :option<'a>
        static member instance (Return, _:List<'a>     ) = fun x -> [x]         :List<'a>
        static member instance (Return, _: 'r -> 'a    ) = fun x -> const'    x : 'r -> 'a
        static member instance (Return, _:'a Async     ) = fun (x:'a) -> async.Return x
        static member instance (Return, _:'a Nullable  ) = fun (x:'a) -> Nullable x
        static member instance (Return, _:Choice<'a,'e>) = fun x -> Choice1Of2     x :Choice<'a,'e>

    type Bind = Bind with
        static member instance (Bind, x:option<_>   , _:option<'b> ) = fun (f:_->option<'b>) -> Option.bind  f x
        static member instance (Bind, x:List<_>     , _:List<'b>   ) = fun (f:_->List<'b>  ) -> List.collect f x
        static member instance (Bind, f:'r->'a      , _:'r->'b     ) = fun (k:_->_->'b) r    -> k (f r) r
    
        static member instance (Bind, x:Async<'a>   , _:'b Async   ) = fun (f:_->Async<'b> ) -> async.Bind(x,f)
        static member instance (Bind, x:Nullable<_> , _:'b Nullable) = fun f ->
            match x with
            | Null -> Nullable()
            | Value v -> f v : Nullable<'b>
        static member instance (Bind, x:Choice<_,'e>, _:Choice<'b,'e>) = fun (k:_->Choice<'b,_>) -> match x with
                                                                                                           | Choice2Of2 l -> Choice2Of2 l
                                                                                                           | Choice1Of2 r -> k r
let inline return' x = Inline.instance Monad.Return x                                                                                                    
let inline (>>=) x (f:_->'R) : 'R = Inline.instance (Monad.Bind, x) f
let inline (=<<) (f:_->'R) x : 'R = Inline.instance (Monad.Bind, x) f


// Do notation ------------------------------------------------------------

type DoNotationBuilder() =
    member inline b.Return(x)    = return' x
    member inline b.Bind(p,rest) = p >>= rest
    member        b.Let (p,rest) = rest p
    member    b.ReturnFrom(expr) = expr
let do' = new DoNotationBuilder()