module Data.Monoid

open Prelude

type Mempty = Mempty with
    static member        (?<-) (_, _Monoid:Mempty, _:'a list ) = []
    static member        (?<-) (_, _Monoid:Mempty, _:'a[]    ) = [||]
    static member        (?<-) (_, _Monoid:Mempty, _:string  ) = ""
    static member        (?<-) (_, _Monoid:Mempty, _:Ordering) = EQ
    static member inline (?<-) (_, _Monoid:Mempty, _: ^A * ^B) = (Mempty ? (Mempty) <- Unchecked.defaultof< ^A>), (Mempty ? (Mempty) <- Unchecked.defaultof< ^B>)

let inline mempty() : ^R = (Mempty ? (Mempty) <- Unchecked.defaultof< ^R>)


type Mappend = Mappend with    
    static member        (?<-) (x:list<_> , _Monoid:Mappend, y)       = List.append  x y
    static member        (?<-) (x:_[]     , _Monoid:Mappend, y)       = Array.append x y
    static member        (?<-) (x:string  , _Monoid:Mappend, y)       = x + y
    static member        (?<-) (x:Ordering, _Monoid:Mappend, y)       = compare' x y
    static member inline (?<-) ((x1,x2)   , _Monoid:Mappend, (y1,y2)) = (x1 ? (Mappend) <- y1) , (x2 ? (Mappend) <- y2)
    
let inline mappend (x:'a) (y:'a) : 'a = x ? (Mappend) <- y


let inline mconcat x =
    let foldR f s lst = List.foldBack f lst s
    x |> foldR mappend (mempty())



type Dual<'a> = Dual of 'a with
    static member inline (?<-) ( _    , _Monoid:Mempty , _:Dual<_>) = Dual (mempty()   )
    static member inline (?<-) (Dual x, _Monoid:Mappend,   Dual y)  = Dual (mappend x y)

type All = All of bool with
    static member (?<-) ( _   , _Monoid:Mempty , _:All  ) = All true
    static member (?<-) (All x, _Monoid:Mappend,   All y) = All (x && y)

type Any = Any of bool with
    static member (?<-) ( _   , _Monoid:Mempty , _:Any  ) = Any false
    static member (?<-) (Any x, _Monoid:Mappend,   Any y) = Any (x || y)

type Sum<'a> = Sum of 'a with
    static member inline (?<-) ( _   , _Monoid:Mempty , _:Sum<_>) = Sum LanguagePrimitives.GenericZero
    static member inline (?<-) (Sum x, _Monoid:Mappend,   Sum y ) = Sum (x + y)

type Product<'a> = Product of 'a with
    static member inline (?<-) ( _       , _Monoid:Mempty , _:Product<_>) = Product LanguagePrimitives.GenericOne
    static member inline (?<-) (Product x, _Monoid:Mappend,   Product y ) = Product (x * y)