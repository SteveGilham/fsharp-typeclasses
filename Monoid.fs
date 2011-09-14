module Data.Monoid

open Prelude

type Mempty = Mempty with
    static member        (?<-) (_, cs:Mempty, t:'a list)  = []
    static member        (?<-) (_, cs:Mempty, t:'a[])     = [||]
    static member        (?<-) (_, cs:Mempty, t:string)   = ""
    static member        (?<-) (_, cs:Mempty, t:Ordering) = EQ
    static member inline (?<-) ( _ , cs:Mempty,  (t1,t2)) = (Mempty ? (Mempty) <- t1), (Mempty ? (Mempty) <- t2)

let inline mempty() : ^R = (Mempty ? (Mempty) <- Unchecked.defaultof< ^R>)


type Mappend = Mappend with    
    static member (?<-) (x:list<_> ,cs:Mappend,y) = List.append  x y
    static member (?<-) (x:_[]     ,cs:Mappend,y) = Array.append x y
    static member (?<-) (x:string  ,cs:Mappend,y) = x + y
    static member (?<-) (x:Ordering,cs:Mappend,y) = compare' x y
    static member inline (?<-) ((x1,x2) , cs:Mappend, (y1,y2)) =  ( x1 ? (Mappend) <- y1) , ( x2 ? (Mappend) <- y2)
    
let inline mappend (x:'a) (y:'a) : 'a = x ? (Mappend) <- y


let inline mconcat x =
    let foldR f s lst = List.foldBack f lst s
    x |> foldR mappend (mempty())



type Dual<'a> = Dual of 'a with
    static member inline (?<-) ( _    , cs:Mempty , Dual t) = Dual ( Mempty ? (Mempty) <- t)
    static member inline (?<-) (Dual x, cs:Mappend, Dual y) = Dual ( y ? (Mappend) <- x)

type All = All of bool with
    static member (?<-) ( _   , cs:Mempty , t:All) = All true
    static member (?<-) (All x, cs:Mappend, All y) = All (x && y)

type Any = Any of bool with
    static member (?<-) ( _   , cs:Mempty , t:Any) = Any false
    static member (?<-) (Any x, cs:Mappend, Any y) = Any (x || y)

type Sum<'a> = Sum of 'a with
    static member inline (?<-) ( _   , cs:Mempty , t:Sum<_>) = Sum LanguagePrimitives.GenericZero
    static member inline (?<-) (Sum x, cs:Mappend, Sum y)    = Sum (x + y)

type Product<'a> = Product of 'a with
    static member inline (?<-) ( _    , cs:Mempty , t:Product<_>) = Product LanguagePrimitives.GenericOne
    static member inline (?<-) (Product x, cs:Mappend, Product y) = Product (x * y)