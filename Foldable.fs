module Data.Foldable
open Prelude
open Data.Monoid

#nowarn "64"

type Foldable = Foldable with
    static member foldr (Foldable, x:option<_>) = fun (f,z) -> match x with |Some t -> f t z |None -> z
    static member foldr (Foldable, x:list<_>  ) = fun (f,z) -> List.foldBack           f x z    
    
    member inline        this.foldMap                    f = fun x -> ((^C or ^a) :(static member foldr: ^C * ^a -> _) (Foldable, x)) (mappend << f,mempty())
    static member inline foldMap   (Foldable, x:option<_>) = fun f -> Foldable.foldMap  f x
    static member inline foldMap   (Foldable, x:list<_>  ) = fun f -> Foldable.foldMap  f x
    static member inline foldMap   (Foldable, x:array<_> ) = fun f -> Array.foldBack (mappend << f) x (mempty())

let inline foldr (f: 'x -> 'y -> 'y) (z:'y) x : 'y = ((^C or ^a) : (static member foldr: ^C * ^a -> _) (Foldable, x)) (f,z)
let inline foldMap f x = ((^C or ^a) : (static member foldMap: ^C * ^a -> _) (Foldable, x)) f

type Foldable with
    member inline        this.foldr                   f = fun z x -> appEndo (foldMap (Endo << f ) x ) z
    static member inline foldr (Foldable, x:array<_>  ) = fun (f,z) -> Foldable.foldr f z x

    member inline this.foldl                         f = fun z t -> appEndo (getDual (foldMap (Dual << Endo << flip f) t)) z
    static member foldl (Foldable, x:option<_>) = fun (f,z) -> match x with |Some t -> f z t |None -> z
    static member foldl (Foldable, x:list<_>  ) = fun (f,z) -> List.fold               f z x
    static member foldl (Foldable, x:array<_> ) = fun (f,z) -> Foldable.foldl        f z x

let inline foldl (f: 'x -> 'y -> 'x) (z:'x) x : 'x = ((^C or ^a) : (static member foldl: ^C * ^a -> _) (Foldable, x)) (f,z)