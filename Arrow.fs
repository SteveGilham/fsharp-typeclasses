module Control.Arrow

open Prelude
open Control.Monad.Base

type Kleisli<'a, 'm> = Kleisli of ('a -> 'm)
let runKleisli (Kleisli f) = f

type Id = Id with
    static member        instance (_Category:Id, _: 'r -> 'r     ) = fun () -> id              : 'r -> 'r
    static member inline instance (_Category:Id, _:Kleisli<'a,'b>) = fun () -> Kleisli return' :Kleisli<'a,'b>

let inline id'() = Inline.instance Id ()

type Comp = Comp with
    static member        instance (_Category:Comp,         f, _) = fun (g: _ -> _) ->          g >>  f
    static member inline instance (_Category:Comp, Kleisli f, _) = fun (Kleisli g) -> Kleisli (g >=> f)

let inline (<<<) f g = Inline.instance (Comp, f) g
let inline (>>>) g f = Inline.instance (Comp, f) g


type Arr = Arr with
    static member        instance (_Arrow:Arr, _: _ -> _     ) = fun (f:_->_) -> f
    static member inline instance (_Arrow:Arr, _:Kleisli<_,_>) = fun  f       -> Kleisli (return' <<< f)

let inline arr f = Inline.instance Arr f

type First = First with
    static member        instance (_Arrow:First, f        , _: 'a -> 'b   ) = fun () -> fun (x,y) -> (f x, y)
    static member inline instance (_Arrow:First, Kleisli f, _:Kleisli<_,_>) = fun () -> Kleisli (fun (b,d) -> f b >>= fun c -> return' (c,d))

let inline first f = Inline.instance (First, f) ()

let inline second f = 
    let swap (x,y) = (y,x)
    arr swap >>> first f >>> arr swap

let inline ( *** ) f g = first f >>> second g
let inline ( &&& ) f g = arr (fun b -> (b,b)) >>> f *** g


type AcEither = AcEither with
    static member inline instance (_ArrowChoice:AcEither, _:Either<_,_>->_) = fun (         f ,          g ) ->          either f g
    static member inline instance (_ArrowChoice:AcEither, _:Kleisli<_,_>  ) = fun ((Kleisli f), (Kleisli g)) -> Kleisli (either f g)

let inline (|||) f g = Inline.instance AcEither (f, g)

type AcMerge = AcMerge with
    static member inline instance (_ArrowChoice:AcMerge, _: _->    Either<_,_>      ) = fun (f, g)  ->  (Left << f) ||| (Right << g)
    static member inline instance (_ArrowChoice:AcMerge, _:Kleisli<Either<'t,'v>,'z>) = fun ((Kleisli (f:'t->'u)), (Kleisli (g:'v->'w))) ->
        Kleisli (f >=> (return' <<< Left)) ||| Kleisli (g >=> (return' <<< Right)) :Kleisli<Either<'t,'v>,'z>

let inline (+++) f g = Inline.instance AcMerge (f, g)


type AcLeft = AcLeft with
    static member inline instance (_ArrowChoice:AcLeft, f:_->_   , _) = fun () ->          f  +++      id
    static member inline instance (_ArrowChoice:AcLeft, Kleisli f, _) = fun () -> (Kleisli f) +++ arr (id'())

let inline left f = Inline.instance (AcLeft, f) ()

type AcRight = AcRight with
    static member inline instance (_ArrowChoice:AcRight, f:_->_   , _) = fun () -> id          +++ f
    static member inline instance (_ArrowChoice:AcRight, Kleisli f, _) = fun () -> arr (id'()) +++ Kleisli f

let inline right f = Inline.instance (AcRight, f) ()


type Apply = Apply with
    static member instance (_ArrowApply:Apply, _: ('a -> 'b) * 'a -> 'b          ) = fun () ->          fun (f,x)          -> f x
    static member instance (_ArrowApply:Apply, _: Kleisli<Kleisli<'a,'b> * 'a,'b>) = fun () -> Kleisli (fun (Kleisli f, x) -> f x)

let inline app() = Inline.instance Apply ()