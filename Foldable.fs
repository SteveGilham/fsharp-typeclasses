module Data.Foldable

type Foldr = Foldr with
    static member (?) (x:option<_>,_Foldable:Foldr) = fun (f,z) -> match x with |Some t -> f t z |None -> z
    static member (?) (x:list<_>  ,_Foldable:Foldr) = fun (f,z) -> List.foldBack           f x z

let inline foldr (f: 'a -> 'b -> 'b) (z:'b) x : 'b = (x ? (Foldr)) (f,z)

type Foldl = Foldl with
    static member (?) (x:option<_>,_Foldable:Foldl) = fun (f,z) -> match x with |Some t -> f z t|None -> z
    static member (?) (x:list<_>  ,_Foldable:Foldl) = fun (f,z) -> List.fold               f z x

let inline foldl (f: 'a -> 'b -> 'a) (z:'a) x : 'a = (x ? (Foldl)) (f,z)