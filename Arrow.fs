module Control.Arrow

open Prelude
open Control.Monad.Base

type Kleisli< 'a, 'm> = Kleisli of ( 'a -> 'm)
let runKleisli (Kleisli f) = f



type Id = Id with
    static member        (?<-) (_, _Category:Id, _: 'r->'r       ) = id              : 'r->'r
    static member inline (?<-) (_, _Category:Id, _:Kleisli<'a,'b>) = Kleisli return' :Kleisli<'a,'b>

let inline id'() : ^R = (() ? (Id) <- defaultof< ^R>)

type Comp = Comp with
    static member        (?<-) (        f, _Category:Comp, g:_->_   ) =          g >>  f
    static member inline (?<-) (Kleisli f, _Category:Comp, Kleisli g) = Kleisli (g >=> f)

let inline (<<<) f g = f ? (Comp) <- g
let inline (>>>) g f = f ? (Comp) <- g


type Arr = Arr with
    static member        (?<-) (f:_->_, _Arrow:Arr, _: _ -> _     ) = f
    static member inline (?<-) (f     , _Arrow:Arr, _:Kleisli<_,_>) = Kleisli (return' <<< f)

let inline arr f : ^R = f ? (Arr) <- defaultof< ^R>

type First = First with
    static member        (?<-) (f        , _Arrow:First, _: 'a -> 'b   ) = fun (x,y) -> (f x, y)
    static member inline (?<-) (Kleisli f, _Arrow:First, _:Kleisli<_,_>) = Kleisli (fun (b,d) -> f b >>>= fun c -> return' (c,d))

let inline first f : ^R = f ? (First) <- defaultof< ^R>

let inline second f = 
    let swap (x,y) = (y,x)
    arr swap >>> first f >>> arr swap

let inline ( *** ) f g = first f >>> second g
let inline ( &&& ) f g = arr (fun b -> (b,b)) >>> f *** g



type AcEither = AcEither with
    static member inline (?<-) ((f,g)                , _ArrowChoice:AcEither, _:Either<_,_>->_) =          either f g
    static member inline (?<-) ((Kleisli f,Kleisli g), _ArrowChoice:AcEither, _:Kleisli<_,_>  ) = Kleisli (either f g)

let inline (|||) f g : ^R = ( (f,g) ? (AcEither) <- defaultof< ^R> )

type AcMerge = AcMerge with
    static member inline (?<-) ( (f,g)                , _ArrowChoice:AcMerge, _:_->Either<_,_>) = (Left << f) ||| (Right << g)
    static member inline (?<-) ((Kleisli f, Kleisli g), _ArrowChoice:AcMerge, _:Kleisli<_,_>  ) =
        Kleisli (f >=> (return' <<< Left)) ||| Kleisli (g >=> (return' <<< Right))

let inline (+++) f g : ^R = (f,g) ? (AcMerge) <- defaultof< ^R>


type AcLeft = AcLeft with
    static member inline (?<-) (f:_->_   , _ArrowChoice:AcLeft, _) =          f  +++      id
    static member inline (?<-) (Kleisli f, _ArrowChoice:AcLeft, _) = (Kleisli f) +++ arr (id'())

let inline left f : ^R = f ? (AcLeft) <- defaultof< ^R>

type AcRight = AcRight with
    static member inline (?<-) (_, _ArrowChoice:AcRight, f:_->_   ) = id          +++ f
    static member inline (?<-) (_, _ArrowChoice:AcRight, Kleisli f) = arr (id'()) +++ Kleisli f

let inline right f = () ? (AcRight) <- f


type Apply = Apply with
    static member (?<-) (_, _ArrowApply:Apply, _: ('a -> 'b) * 'a -> 'b            ) =          fun (f,x)          -> f x
    static member (?<-) (_, _ArrowApply:Apply, _: Kleisli<(Kleisli<'a,'b> * 'a),'b>) = Kleisli (fun (Kleisli f, x) -> f x)

let inline app() : ^R = () ? (Apply) <- defaultof< ^R>