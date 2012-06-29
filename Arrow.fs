module Control.Arrow

open Prelude
open Control.Monad.Base

#nowarn "64"

type Kleisli< 'a, 'm> = Kleisli of ( 'a -> 'm)
let runKleisli (Kleisli f) = f


type Category = Category with
    static member        id (Category, _: 'r->'r       ) = id              : 'r->'r
    static member inline id (Category, _:Kleisli<'a,'b>) = Kleisli return' :Kleisli<'a,'b>

    static member        comp (Category,         f, g:_->_   ) =          g >>  f
    static member inline comp (Category, Kleisli f, Kleisli g) = Kleisli (g >=> f)

let inline id'() : ^R = ((^C or       ^R) : (static member id : ^C * ^R      -> _) (Category, defaultof< ^R>))
let inline (<<<) f g =  ((^C or ^a or ^b) : (static member comp : ^C * ^a * ^b -> _) (Category, f, g))
let inline (>>>) g f =  ((^C or ^a or ^b) : (static member comp : ^C * ^a * ^b -> _) (Category, f, g))


type Arrow = Arrow with
    static member        arr (Arrow, f:_->_, _: _ -> _     ) = f
    static member inline arr (Arrow, f     , _:Kleisli<_,_>) = Kleisli (return' <<< f)

    static member        first (Arrow, f        , _: 'a -> 'b   ) = fun (x,y) -> (f x, y)
    static member inline first (Arrow, Kleisli f, _:Kleisli<_,_>) = Kleisli (fun (b,d) -> f b >>>= fun c -> return' (c,d))


let inline arr   f : ^R = ((^C or ^a or ^R) : (static member arr   : ^C * ^a * ^R -> _) (Arrow,f,defaultof< ^R>))
let inline first f : ^R = ((^C or ^a or ^R) : (static member first : ^C * ^a * ^R -> _) (Arrow,f,defaultof< ^R>))


let inline second f = 
    let swap (x,y) = (y,x)
    arr swap >>> first f >>> arr swap

let inline ( *** ) f g = first f >>> second g
let inline ( &&& ) f g = arr (fun b -> (b,b)) >>> f *** g


type ArrowChoice = ArrowChoice with
    static member inline acEither (ArrowChoice, (f,g)                , _:Either<_,_>->_) =          either f g
    static member inline acEither (ArrowChoice, (Kleisli f,Kleisli g), _:Kleisli<_,_>  ) = Kleisli (either f g)

let inline (|||) f g : ^R = ((^C or ^a or ^R) : (static member acEither : ^C * ^a * ^R -> _) (ArrowChoice,(f,g),defaultof< ^R>))

type ArrowChoice with
    static member inline acMerge (ArrowChoice, (f,g)                 , _:_->Either<_,_>) = (Left << f) ||| (Right << g)
    static member inline acMerge (ArrowChoice, (Kleisli f, Kleisli g), _:Kleisli<_,_>  ) =
        Kleisli (f >=> (return' <<< Left)) ||| Kleisli (g >=> (return' <<< Right))

let inline (+++) f g : ^R = ((^C or ^a or ^R) : (static member acMerge  : ^C * ^a * ^R -> _) (ArrowChoice,(f,g),defaultof< ^R>))


type ArrowChoice with
    static member inline acLeft (ArrowChoice, f:_->_   ) =          f  +++      id
    static member inline acLeft (ArrowChoice, Kleisli f) = (Kleisli f) +++ arr (id'())

    static member inline acRight (ArrowChoice, f:_->_   ) = id          +++ f
    static member inline acRight (ArrowChoice, Kleisli f) = arr (id'()) +++ Kleisli f

let inline left  f : ^R = ((^C or ^a or ^R) : (static member acLeft  : ^C * ^a * ^R -> _) (ArrowChoice, f, defaultof< ^R>))
let inline right f : ^R = ((^C or ^a or ^R) : (static member acRight : ^C * ^a * ^R -> _) (ArrowChoice, f, defaultof< ^R>))



type ArrowApply = ArrowApply with
    static member app (ArrowApply, _: ('a -> 'b) * 'a -> 'b            ) =          fun (f,x)          -> f x
    static member app (ArrowApply, _: Kleisli<(Kleisli<'a,'b> * 'a),'b>) = Kleisli (fun (Kleisli f, x) -> f x)

let inline app() : ^R = ((^C or ^R) : (static member app : ^C * ^R -> _) (ArrowApply, defaultof< ^R>))