module Control.Arrow

open Prelude
open Control.Monad.Base

type Kleisli<'a, 'm> = Kleisli of ('a -> 'm)
let runKleisli (Kleisli f) = f


type Id = Id with
    static member        (?<-) (_Category:Id, _: 'r -> 'r     , _) = id              : 'r -> 'r
    static member inline (?<-) (_Category:Id, _:Kleisli<'a,'b>, _) = Kleisli return' :Kleisli<'a,'b>

let inline id'() : ^R = Id ? (defaultof< ^R>) <- ()

type Comp = Comp with
    static member        (?<-) (_Category:Comp,         f, g: _ -> _) =          g >>  f
    static member inline (?<-) (_Category:Comp, Kleisli f, Kleisli g) = Kleisli (g >=> f)

let inline (<<<) f g = Comp ? (f) <- g
let inline (>>>) g f = Comp ? (f) <- g


type Arr = Arr with
    static member        (?<-) (_Arrow:Arr, _: _ -> _     , _) = fun (f:_->_) -> f
    static member inline (?<-) (_Arrow:Arr, _:Kleisli<_,_>, _) = fun (f     ) -> Kleisli (return' <<< f)

let inline arr f : ^R = (Arr ? (defaultof< ^R>) <- ()) f

type First = First with
    static member        (?<-) (_Arrow:First, f        , _: 'a -> 'b   ) = fun (x,y) -> (f x, y)
    static member inline (?<-) (_Arrow:First, Kleisli f, _:Kleisli<_,_>) = Kleisli (fun (b,d) -> f b >>= fun c -> return' (c,d))

let inline first f : ^R = First ? (f) <- defaultof< ^R>

let inline second f = 
    let swap (x,y) = (y,x)
    arr swap >>> first f >>> arr swap

let inline ( *** ) f g = first f >>> second g
let inline ( &&& ) f g = arr (fun b -> (b,b)) >>> f *** g


type AcEither = AcEither with
    static member inline (?<-) (_ArrowChoice:AcEither, _:Either<_,_>->_, _) = fun (        f,        g) ->          either f g
    static member inline (?<-) (_ArrowChoice:AcEither, _:Kleisli<_,_>  , _) = fun (Kleisli f,Kleisli g) -> Kleisli (either f g)

let inline (|||) f g : ^R = (AcEither ? (defaultof< ^R>) <- ()) (f,g)

type AcMerge = AcMerge with
    static member inline (?<-) (_ArrowChoice:AcMerge, _:_->Either<_,_>, _) = fun (        f,         g) -> (Left << f) ||| (Right << g)
    static member inline (?<-) (_ArrowChoice:AcMerge, _:Kleisli<_,_>  , _) = fun (Kleisli f, Kleisli g) ->
        Kleisli (f >=> (return' <<< Left)) ||| Kleisli (g >=> (return' <<< Right))

let inline (+++) f g : ^R = (AcMerge ? (defaultof< ^R>) <- ()) (f,g)


type AcLeft = AcLeft with
    static member inline (?<-) (_ArrowChoice:AcLeft, f:_->_   , _) =          f  +++      id
    static member inline (?<-) (_ArrowChoice:AcLeft, Kleisli f, _) = (Kleisli f) +++ arr (id'())

let inline left f : ^R = AcLeft ? (f) <- defaultof< ^R>

type AcRight = AcRight with
    static member inline (?<-) (_ArrowChoice:AcRight, f:_->_   , _) = id          +++ f
    static member inline (?<-) (_ArrowChoice:AcRight, Kleisli f, _) = arr (id'()) +++ Kleisli f

let inline right f = AcRight ? (f) <- ()


type Apply = Apply with
    static member (?<-) (_ArrowApply:Apply, _: ('a -> 'b) * 'a -> 'b            , _) =          fun (f,x)          -> f x
    static member (?<-) (_ArrowApply:Apply, _: Kleisli<(Kleisli<'a,'b> * 'a),'b>, _) = Kleisli (fun (Kleisli f, x) -> f x)

let inline app() : ^R = Apply ? (defaultof< ^R>) <- ()