module Data.Foldable
open Prelude
open Data.Monoid

#nowarn "64"

type Foldable = Foldable with
    static member foldr (_:Foldable, x:Maybe<_>) = fun (f,z) -> match x with |Just t -> f t z |Nothing -> z
    static member foldr (_:Foldable, x:List<_> ) = fun (f,z) -> List.foldBack           f x z    

let inline foldr (f: 'x -> 'y -> 'y) (z:'y) x : 'y = ((^C or ^a) : (static member foldr: ^C * ^a -> _) (Foldable, x)) (f,z)

type Foldable with    
    static member inline foldMap f = foldr (mappend << f) (mempty())
    static member inline foldMap (_:Foldable, x:Maybe<_>) = fun f -> Foldable.foldMap  f x
    static member inline foldMap (_:Foldable, x:List<_> ) = fun f -> Foldable.foldMap  f x
    static member inline foldMap (_:Foldable, x:array<_>) = fun f -> Array.foldBack (mappend << f) x (mempty())

let inline foldMap f x = ((^C or ^a) : (static member foldMap: ^C * ^a -> _) (Foldable, x)) f

type Foldable with
    static member inline foldr f = fun z x -> appEndo (foldMap (Endo << f ) x ) z
    static member inline foldl f = fun z t -> appEndo (getDual (foldMap (Dual << Endo << flip f) t)) z
    static member inline foldr (_:Foldable, x:array<_>) = fun (f,z) -> Foldable.foldr f z x    
    static member foldl (_:Foldable, x:Maybe<_>) = fun (f,z) -> match x with |Just t -> f z t |Nothing -> z
    static member foldl (_:Foldable, x:List<_> ) = fun (f,z) -> List.fold               f z x
    static member foldl (_:Foldable, x:array<_>) = fun (f,z) -> Foldable.foldl          f z x

let inline foldl (f: 'x -> 'y -> 'x) (z:'x) x : 'x = ((^C or ^a) : (static member foldl: ^C * ^a -> _) (Foldable, x)) (f,z)