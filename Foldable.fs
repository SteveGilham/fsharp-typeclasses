module Data.Foldable
open Prelude
open Data.Monoid

module Foldable =
    type Foldr = Foldr with
        static member instance (Foldr, x:option<_>, _) = fun (f,z) -> match x with |Some t -> f t z |None -> z
        static member instance (Foldr, x:List<_>  , _) = fun (f,z) -> List.foldBack           f x z

    let inline foldMap f x = Inline.instance (Foldr, x) (mappend << f, mempty())
   
    type FoldMap = FoldMap with
        static member inline instance (FoldMap, x:option<_>, _) = fun f -> foldMap f x
        static member inline instance (FoldMap, x:List<_>  , _) = fun f -> foldMap f x
        static member inline instance (FoldMap, x:array<_> , _) = fun f -> Array.foldBack (mappend << f) x (mempty())

    let inline foldr f z x = 
        let inline foldMap f x = Inline.instance (FoldMap, x) f
        appEndo (foldMap (Endo << f ) x ) z

    let inline foldl f z t = 
        let inline foldMap f x = Inline.instance (FoldMap, x) f
        appEndo (getDual (foldMap (Dual << Endo << flip f) t)) z

    type Foldr with
        static member inline instance (Foldr, x:array<_>, _) = fun (f,z) -> foldr f z x

    type Foldl = Foldl with
        static member instance (Foldl, x:option<_>, _) = fun (f,z) -> match x with |Some t -> f z t |None -> z
        static member instance (Foldl, x:List<_>  , _) = fun (f,z) -> List.fold               f z x
        static member instance (Foldl, x:array<_> , _) = fun (f,z) -> foldl          f z x

let inline foldr (f: 'a -> 'b -> 'b) (z:'b) x :'b = Inline.instance (Foldable.Foldr, x) (f,z)
let inline foldMap f x = Inline.instance (Foldable.FoldMap, x) f
let inline foldl (f: 'a -> 'b -> 'a) (z:'a) x :'a = Inline.instance (Foldable.Foldl, x) (f,z)