module Data.Monoid


type Mempty = Mempty with
    static member (?<-) (_, cs:Mempty, t:'a list) =  []
    static member (?<-) (_, cs:Mempty, t:'a[])    =  [||]

let inline mempty() : ^R = (Mempty ? (Mempty) <- Unchecked.defaultof< ^R> )


type Mappend = Mappend with
    static member (?<-) (x:list<_> ,cs:Mappend,y) = List.append  x y
    static member (?<-) (x:_[]     ,cs:Mappend,y) = Array.append x y
    
let inline mappend x y = x ? (Mappend) <- y


let inline mconcat x =
    let foldrForList  f s lst = List.foldBack f lst s
    x |> foldrForList mappend (mempty())



type All = All of bool with
    static member inline (?<-) (_     , cs:Mempty , t:All) = All true
    static member inline (?<-) (All x , cs:Mappend, All y)   =  All (x && y)

type Any = Any of bool with
    static member inline (?<-) (_     , cs:Mempty , t:Any) = Any false
    static member inline (?<-) (Any x , cs:Mappend, Any y)   =  Any (x || y)

type Sum<'a> = Sum of 'a with
    static member inline (?<-) (_     , cs:Mempty , t:Sum<_>) = Sum LanguagePrimitives.GenericZero
    static member inline (?<-) (Sum x , cs:Mappend, Sum y)   = Sum (x + y)

type Product<'a> = Product of 'a with
    static member inline (?<-) (_     , cs:Mempty , t:Product<_>) = Product LanguagePrimitives.GenericOne
    static member inline (?<-) (Product x, cs:Mappend, Product y)   = Product (x * y)