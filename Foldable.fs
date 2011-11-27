module Data.Foldable

type Foldr = Foldr with
    static member (?<-) (_, _Foldable:Foldr, x:option<_>) = fun (f,z) -> match x with |Some t -> f t z |None -> z
    static member (?<-) (_, _Foldable:Foldr, x:list<_>  ) = fun (f,z) -> List.foldBack           f x z

let inline foldr (f: 'a -> 'b -> 'b) (z:'b) x : 'b = (() ? (Foldr) <- x) (f,z)

type Foldl = Foldl with
    static member (?<-) (_, _Foldable:Foldl, x:option<_>) = fun (f,z) -> match x with |Some t -> f z t |None -> z
    static member (?<-) (_, _Foldable:Foldl, x:list<_>  ) = fun (f,z) -> List.fold               f z x

let inline foldl (f: 'a -> 'b -> 'a) (z:'a) x : 'a = (() ? (Foldl) <- x) (f,z)