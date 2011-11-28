module Data.Foldable
open Data.Monoid

type Foldable = Foldable with
    static member (?<-) (_, Foldable, x:option<_>) = fun (f,z) -> match x with |Some t -> f t z |None -> z
    static member (?<-) (_, Foldable, x:list<_>  ) = fun (f,z) -> List.foldBack           f x z

    static member inline ($)   (Foldable, f:_->option<_>) = fun x -> (() ? (Foldable) <- x) (mappend << f,mempty())
    static member inline ($)   (Foldable, f:_->list<_>  ) = fun x -> (() ? (Foldable) <- x) (mappend << f,mempty())

let inline foldr (f: 'a -> 'b -> 'b) (z:'b) x : 'b = (() ? (Foldable) <- x) (f,z)
let inline foldMap f = Foldable $ f

type Foldl = Foldl with
    static member (?<-) (_, _Foldable:Foldl, x:option<_>) = fun (f,z) -> match x with |Some t -> f z t |None -> z
    static member (?<-) (_, _Foldable:Foldl, x:list<_>  ) = fun (f,z) -> List.fold               f z x

    

let inline foldl (f: 'a -> 'b -> 'a) (z:'a) x : 'a = (() ? (Foldl) <- x) (f,z)