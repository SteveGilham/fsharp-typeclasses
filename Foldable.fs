module Data.Foldable
open Prelude
open Data.Monoid

type Foldr = Foldr with
    static member (?<-) (_, Foldr, x:option<_>) = fun (f,z) -> match x with |Some t -> f t z |None -> z
    static member (?<-) (_, Foldr, x:list<_>  ) = fun (f,z) -> List.foldBack           f x z    
    
type FoldMap = FoldMap with
    member inline        this.Base                    f x = (() ? (Foldr) <- x) (mappend << f,mempty())
    static member inline (?<-)   (_,FoldMap, x:option<_>) = fun f -> FoldMap.Base  f x
    static member inline (?<-)   (_,FoldMap, x:list<_>  ) = fun f -> FoldMap.Base  f x
    static member inline (?<-)   (_,FoldMap, x:array<_> ) = fun f -> Array.foldBack (mappend << f) x (mempty())

let inline foldr (f: 'a -> 'b -> 'b) (z:'b) x : 'b = (() ? (Foldr) <- x) (f,z)
let inline foldMap f x = (() ? (FoldMap) <- x) f

type Foldr with
    member inline        this.Base                f z x = appEndo (foldMap (Endo << f ) x ) z
    static member inline (?<-) (_, Foldr, x:array<_>  ) = fun (f,z) -> Foldr.Base  f z x


type Foldl = Foldl with
    member inline this.Base                         f z t = appEndo (getDual (foldMap (Dual << Endo << flip f) t)) z
    static member (?<-) (_, _Foldable:Foldl, x:option<_>) = fun (f,z) -> match x with |Some t -> f z t |None -> z
    static member (?<-) (_, _Foldable:Foldl, x:list<_>  ) = fun (f,z) -> List.fold               f z x
    static member (?<-) (_, _Foldable:Foldl, x:array<_> ) = fun (f,z) -> Foldl.Foldl.Base        f z x

let inline foldl (f: 'a -> 'b -> 'a) (z:'a) x : 'a = (() ? (Foldl) <- x) (f,z)