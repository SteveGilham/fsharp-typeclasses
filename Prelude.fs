module Prelude

let inline defaultof< ^T> = Unchecked.defaultof< ^T>

let flip f x y = f y x
let const' k _ = k

let (</) = (|>)
let (/>) = flip
let (++) = (@)

type DeReference = DeReference with
    static member (?<-) (_, DeReference, a:'a ref) = !a
    static member (?<-) (_, DeReference, a:string) = a.ToCharArray() |> Array.toList
    static member (?<-) (_, DeReference, a:DeReference) = DeReference

let inline (!) a = () ? (DeReference) <- a

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


#nowarn "64"

// Num class --------------------------------------------------------------

open System.Numerics
type Integer = bigint

type Num = Num with
    static member fromInteger (Num, _:sbyte     ) = fun (x:Integer) -> sbyte           x
    static member fromInteger (Num, _:int16     ) = fun (x:Integer) -> int16           x
    static member fromInteger (Num, _:int32     ) = fun (x:Integer) -> int             x
    static member fromInteger (Num, _:int64     ) = fun (x:Integer) -> int64           x
    static member fromInteger (Num, _:nativeint ) = fun (x:Integer) -> nativeint  (int x)
    static member fromInteger (Num, _:byte      ) = fun (x:Integer) -> byte            x
    static member fromInteger (Num, _:uint16    ) = fun (x:Integer) -> uint16          x
    static member fromInteger (Num, _:uint32    ) = fun (x:Integer) -> uint32          x
    static member fromInteger (Num, _:uint64    ) = fun (x:Integer) -> uint64          x
    static member fromInteger (Num, _:unativeint) = fun (x:Integer) -> unativeint (int x)
    static member fromInteger (Num, _:bigint    ) = fun (x:Integer) ->                 x
    static member fromInteger (Num, _:float     ) = fun (x:Integer) -> float           x
    static member fromInteger (Num, _:float32   ) = fun (x:Integer) -> float32         x    
    static member fromInteger (Num, _:decimal   ) = fun (x:Integer) -> decimal         x
    static member fromInteger (Num, _:Complex   ) = fun (x:Integer) -> Complex (float  x, 0.0)


let inline fromInteger (x:Integer) :^Num = ((^C or ^Num) : (static member fromInteger: ^C * ^Num -> _) (Num, defaultof< ^Num>)) x

type Num with
    static member inline abs    (Num, _:^t when ^t: null and ^t: struct) = id
    static member inline abs    (Num, x:^t        ) = abs x
    static member        abs    (Num, x:byte      ) =     x
    static member        abs    (Num, x:uint16    ) =     x
    static member        abs    (Num, x:uint32    ) =     x
    static member        abs    (Num, x:uint64    ) =     x
    static member        abs    (Num, x:unativeint) =     x
    static member        abs    (Num, x:Complex   ) = Complex(x.Magnitude, 0.0)

    static member inline signum (Num, _:^t when ^t: null and ^t: struct) = id
    static member inline signum (Num, x:^t        ) = fromInteger (bigint (sign x)) :^t
    static member        signum (Num, x:byte      ) = if x = 0uy then 0uy else 1uy
    static member        signum (Num, x:uint16    ) = if x = 0us then 0us else 1us
    static member        signum (Num, x:uint32    ) = if x = 0u  then 0u  else 1u
    static member        signum (Num, x:uint64    ) = if x = 0UL then 0UL else 1UL
    static member        signum (Num, x:unativeint) = if x = 0un then 0un else 1un
    static member        signum (Num, x:Complex   ) =
        if x.Magnitude = 0.0 then Complex.Zero
        else Complex(x.Real / x.Magnitude, x.Imaginary / x.Magnitude)

let inline abs    (x:^Num) :^Num  = ((^C or ^Num) : (static member abs   : ^C * ^Num -> _) (Num, x))
let inline signum (x:^Num) :^Num  = ((^C or ^Num) : (static member signum: ^C * ^Num -> _) (Num, x))

let inline (+) (a:'Num) (b:'Num) :'Num = a + b
let inline (-) (a:'Num) (b:'Num) :'Num = a - b
let inline (*) (a:'Num) (b:'Num) :'Num = a * b

type Num with
    static member inline negate (Num, _:^t when ^t: null and ^t: struct) = id
    static member inline negate (Num, x:^t        ) = -x
    static member        negate (Num, x:byte      ) = 0uy - x
    static member        negate (Num, x:uint16    ) = 0us - x
    static member        negate (Num, x:uint32    ) = 0u  - x
    static member        negate (Num, x:uint64    ) = 0UL - x
    static member        negate (Num, x:unativeint) = 0un - x

let inline negate (x:^Num) :^Num = ((^C or ^Num) : (static member negate: ^C * ^Num -> _) (Num, x))
let inline (~-)   (x:^Num) :^Num = ((^C or ^Num) : (static member negate: ^C * ^Num -> _) (Num, x))


// Integral class ---------------------------------------------------------

type Integral = Integral with
    static member toInteger(Integral, x:sbyte     ) = bigint (int x)
    static member toInteger(Integral, x:int16     ) = bigint (int x)
    static member toInteger(Integral, x:int32     ) = bigint      x
    static member toInteger(Integral, x:int64     ) = bigint      x
    static member toInteger(Integral, x:nativeint ) = bigint (int x)
    static member toInteger(Integral, x:byte      ) = bigint (int x)
    static member toInteger(Integral, x:uint16    ) = bigint (int x)
    static member toInteger(Integral, x:uint32    ) = bigint      x
    static member toInteger(Integral, x:uint64    ) = bigint      x
    static member toInteger(Integral, x:unativeint) = bigint (int x)
    static member toInteger(Integral, x:bigint    ) =             x

let inline toInteger (x:^Integral) :Integer = ((^C or ^Integral) : (static member toInteger: ^C * ^Integral -> _) (Integral, x))

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
        if b = zero then failwith "Ratio.%: zero denominator"
        let (a,b) = if b < zero then (negate a, negate b) else (a,b)
        let gcd = gcd a b
        Ratio (a </quot/> gcd, b </quot/> gcd)

    let inline Ratio (x,y) = x % y
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

type Num with
    static member inline abs         (Num, r:Ratio<_>) = (abs    (numerator r)) % (denominator r)
    static member inline signum      (Num, r:Ratio<_>) = (signum (numerator r)) % 1G
    static member inline fromInteger (Num, _:Ratio<_>) = fun (x:Integer) -> fromInteger x % 1G
    static member inline negate      (Num, r:Ratio<_>) = -(numerator r) % (denominator r)


// Fractional class -------------------------------------------------------

type Fractional = Fractional with
    static member        fromRational (Fractional, _:float   ) = fun (r:Rational) -> float   (numerator r) / float   (denominator r)
    static member        fromRational (Fractional, _:float32 ) = fun (r:Rational) -> float32 (numerator r) / float32 (denominator r)    
    static member        fromRational (Fractional, _:decimal ) = fun (r:Rational) -> decimal (numerator r) / decimal (denominator r)
    static member inline fromRational (Fractional, _:Ratio<_>) = fun (r:Rational) -> fromIntegral  (numerator r) % fromIntegral (denominator r)
    static member        fromRational (Fractional, _:Complex ) = fun (r:Rational) -> Complex(float (numerator r) / float (denominator r), 0.0)

let inline fromRational (x:Rational) :^Fractional = ((^C or ^Fractional) : (static member fromRational: ^C * ^Fractional -> _) (Fractional, defaultof< ^Fractional>)) x

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

type RealFrac = RealFrac with
    static member        properFraction (RealFrac, x:float   ) = let t = truncate x in (bigint (decimal t), x - t)
    static member        properFraction (RealFrac, x:float32 ) = let t = truncate x in (bigint (decimal t), x - t)
    static member        properFraction (RealFrac, x:decimal ) = let t = truncate x in (bigint          t , x - t)
    static member inline properFraction (RealFrac, r:Ratio<_>) =
        let (a,b) = (numerator r, denominator r)
        let (i,f) = quotRem a b
        (i, f % b)

let inline properFraction (x:'RealFrac) : 'Integral * 'RealFrac =
    let (a, b:'RealFrac) = ((^C or ^RealFrac) : (static member properFraction: ^C * ^RealFrac -> _) (RealFrac, x))
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

type Floating = Floating with
    static member pi (Floating, _:float32) = 3.14159274f
    static member pi (Floating, _:float  ) = System.Math.PI
    static member pi (Floating, _:Complex) = Complex(System.Math.PI, 0.0)

let inline pi() : ^Floating = ((^C or ^Floating) : (static member pi: ^C * ^Floating -> _) (Floating, defaultof< ^Floating>))

let inline ( **) a (b:'Floating) :'Floating = a ** b
let inline sqrt    (x:'Floating) :'Floating = sqrt x

let inline asinh x :'Floating = log (x + sqrt (1G+x*x))
let inline acosh x :'Floating = log (x + (x+1G) * sqrt ((x-1G)/(x+1G)))
let inline atanh x :'Floating = (1G/2G) * log ((1G+x) / (1G-x))

let inline logBase x y :'Floating = log y / log x


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

type Functor = Functor with
    static member fmap (Functor, x:Maybe<_>     ) = fun f -> Option.map f x
    static member fmap (Functor, x:List<_>      ) = fun f -> List.map   f x
    static member fmap (Functor, x:IO<_>        ) = fun f -> primbindIO x (primretIO << f)
    static member fmap (Functor, g:_->_         ) = (>>) g
    static member fmap (Functor, e:Either<'a,'b>) = fun f ->
        match e with
        | (Left x ) -> Left x
        | (Right y) -> Right (f y)

let inline fmap f x =   ((^C or ^a) : (static member fmap : ^C * ^a -> _) (Functor, x)) f


// Monad class ------------------------------------------------------------

type Monad = Monad with
    static member return' (Monad, _:Maybe<'a>    ) = fun x -> Some x      :Maybe<'a>
    static member return' (Monad, _:List<'a>     ) = fun x -> [x]         :List<'a>
    static member return' (Monad, _:IO<'a>       ) = fun x -> primretIO x :IO<'a>
    static member return' (Monad, _:'r -> 'a     ) = fun x -> const'    x :'r -> 'a
    static member return' (Monad, _:Either<'e,'a>) = fun x -> Right     x :Either<'e,'a>
     
    static member bind (Monad, x:Maybe<_>    , _:Maybe<'b>    ) = fun (f:_->Maybe<'b>   ) -> Option.bind  f x
    static member bind (Monad, x:List<_>     , _:List<'b>     ) = fun (f:_->List<'b>    ) -> List.collect f x
    static member bind (Monad, x:IO<_>       , _:IO<'b>       ) = fun (f:_->IO<'b>      ) -> primbindIO x f
    static member bind (Monad, f:'r->_       , _:'r->'b       ) = fun (k:_->_->'b) r      -> k (f r) r
    static member bind (Monad, x:Either<'e,_>, _:Either<'e,'b>) = fun (k:_->Either<_,'b>) -> match x with
                                                                                              | Left  l -> Left l
                                                                                              | Right r -> k r

let inline return' x : ^R = ((^C or       ^R) : (static member return' : ^C   * ^R -> _) (Monad,    defaultof< ^R>)) x
let inline (>>=) x f : ^R = ((^C or ^a or ^R) : (static member bind : ^C * ^a * ^R -> _) (Monad, x, defaultof< ^R>)) f


// Do notation ------------------------------------------------------------

type DoNotationBuilder() =
    member inline b.Return(x)    = return' x
    member inline b.Bind(p,rest) = p >>= rest
    member        b.Let (p,rest) = rest p
    member    b.ReturnFrom(expr) = expr
let do' = new DoNotationBuilder()