module Data.Foldable
open Prelude
open Data.Monoid

type Foldr = Foldr with
    static member (?<-) (_, _Foldable:Foldr, x:Maybe<_>) = fun (f,z) -> match x with |Just t -> f t z |Nothing -> z
    static member (?<-) (_, _Foldable:Foldr, x:List<_> ) = fun (f,z) -> List.foldBack           f x z


type Foldable = Foldable with 
    static member inline foldMap f x = (() ? (Foldr) <- x) (mappend << f,mempty())
    
type FoldMap = FoldMap with
    static member inline (?<-)   (_, _Foldable:FoldMap, x:Maybe<_>) = fun f -> Foldable.foldMap f x
    static member inline (?<-)   (_, _Foldable:FoldMap, x:List<_> ) = fun f -> Foldable.foldMap f x
    static member inline (?<-)   (_, _Foldable:FoldMap, x:array<_>) = fun f -> Array.foldBack (mappend << f) x (mempty())

let inline foldr (f: 'a -> 'b -> 'b) (z:'b) x : 'b = (() ? (Foldr) <- x) (f,z)
let inline foldMap f x = (() ? (FoldMap) <- x) f


type Foldable with 
    static member inline foldr f z x = appEndo (foldMap (Endo << f ) x ) z
    static member inline foldl f z t = appEndo (getDual (foldMap (Dual << Endo << flip f) t)) z

type Foldr with
    static member inline (?<-) (_, _Foldable:Foldr, x:array<_>) = fun (f,z) -> Foldable.foldr f z x

type Foldl = Foldl with
    static member (?<-) (_, _Foldable:Foldl, x:Maybe<_>) = fun (f,z) -> match x with |Just t -> f z t |Nothing -> z
    static member (?<-) (_, _Foldable:Foldl, x:List<_> ) = fun (f,z) -> List.fold               f z x
    static member (?<-) (_, _Foldable:Foldl, x:array<_>) = fun (f,z) -> Foldable.foldl          f z x

let inline foldl (f: 'a -> 'b -> 'a) (z:'a) x : 'a = (() ? (Foldl) <- x) (f,z)