module Data.Foldable
open Prelude
open Data.Monoid

type Foldr = Foldr with
    static member (?<-) (_, Foldr, x:option<_>) = fun (f,z) -> match x with |Some t -> f t z |None -> z
    static member (?<-) (_, Foldr, x:list<_>  ) = fun (f,z) -> List.foldBack           f x z
    //static member inline (?<-) (_, Foldable, x:array<_>  ) = fun (f,z) -> appEndo (Foldable $ x <| (Endo << f ) ) z

    
    
type FoldMap = FoldMap with
    member inline this.foldMap f x = (() ? (Foldr) <- x) (mappend << f,mempty())
    static member inline ($)   (FoldMap, x:option<_>) = fun f -> FoldMap.foldMap f x
    static member inline ($)   (FoldMap, x:list<_>  ) = fun f -> FoldMap.foldMap f x
    static member inline ($)   (FoldMap, x:array<_> ) =
        printfn "foldmap for array"
        fun f -> Array.foldBack (mappend << f) x (mempty())

let inline foldr (f: 'a -> 'b -> 'b) (z:'b) x : 'b = (() ? (Foldr) <- x) (f,z)
let inline foldMap f x = FoldMap $ x <| f

type Foldr with
    member inline this.foldr f z x = appEndo (foldMap (Endo << f ) x ) z
    static member inline (?<-) (_, Foldr, x:array<_>  ) = 
        printfn "last overload"
        fun (f,z) -> Foldr.foldr f z x


type Foldl = Foldl with
    member inline this.foldl f z t = appEndo (getDual (foldMap (Dual << Endo << flip f) t)) z
    static member (?<-) (_, _Foldable:Foldl, x:option<_>) = fun (f,z) -> match x with |Some t -> f z t |None -> z
    static member (?<-) (_, _Foldable:Foldl, x:list<_>  ) = fun (f,z) -> List.fold               f z x
    static member (?<-) (_, _Foldable:Foldl, x:array<_> ) = fun (f,z) -> _Foldable.foldl f z x

let inline foldl (f: 'a -> 'b -> 'a) (z:'a) x : 'a = (() ? (Foldl) <- x) (f,z)