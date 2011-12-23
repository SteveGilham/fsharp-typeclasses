module Data.Monoid

open Prelude

type Mempty = Mempty with    
    static member        (?<-) (_, _Monoid:Mempty, _:'a list  ) = []    :'a list
    static member        (?<-) (_, _Monoid:Mempty, _:'a option) = None  :'a option
    static member        (?<-) (_, _Monoid:Mempty, _:'a[]     ) = [||]  :'a[]
    static member        (?<-) (_, _Monoid:Mempty, _:string   ) = ""
    static member        (?<-) (_, _Monoid:Mempty, _:Ordering ) = EQ
    static member        (?<-) (_, _Monoid:Mempty, _:unit     ) = ()
    static member inline (?<-) (_, _Monoid:Mempty, _: ^A * ^B ) =
        (() ? (Mempty) <- Unchecked.defaultof< ^A>) , (() ? (Mempty) <- Unchecked.defaultof< ^B>) : ^A * ^B

let inline mempty() : ^R = (() ? (Mempty) <- Unchecked.defaultof< ^R>)


type Mappend = Mappend with        
    static member        (?<-) (x:list<_>  , _Monoid:Mappend, y      ) = List.append  x y        
    static member inline (?<-) (x:option<_>, _Monoid:Mappend, y      ) = 
        match (x,y) with
        | (Some a,Some b) -> Some (a ? (Mappend) <- b)
        | (Some a,None  ) -> Some a
        | (None  ,Some b) -> Some b
        | _               -> None
    static member        (?<-) (x:_[]      , _Monoid:Mappend, y      ) = Array.append x y
    static member        (?<-) (x:string   , _Monoid:Mappend, y      ) = x + y
    static member        (?<-) (x:Ordering , _Monoid:Mappend, y      ) =
        match (x,y) with
        | (LT,_) -> LT
        | (EQ,a) -> a
        | (GT,_) -> GT
    static member        (?<-) (()         , _Monoid:Mappend, _:unit ) = ()    
    static member inline (?<-) ((x1:'a,x2:'b)    , _Monoid:Mappend, (y1:'a,y2:'b)) = 
        (x1 ? (Mappend) <- y1) , (x2 ? (Mappend) <- y2) :'a*'b
    
let inline mappend (x:'a) (y:'a) : 'a = x ? (Mappend) <- y


let inline mconcat x =
    let foldR f s lst = List.foldBack f lst s
    foldR mappend (mempty()) x



type Dual<'a> = Dual of 'a with
    static member inline (?<-) (_     , _Monoid:Mempty , _:Dual<'m>) = Dual (mempty()   ) :Dual<'m>
    static member inline (?<-) (Dual x, _Monoid:Mappend,   Dual y  ) = Dual (mappend y x)
let getDual (Dual x) = x

type Endo<'a> = Endo of ('a -> 'a) with
    static member        (?<-) (_     , _Monoid:Mempty , _:Endo<'m>) = Endo id  :Endo<'m>
    static member        (?<-) (Endo f, _Monoid:Mappend,   Endo g  ) = Endo (f << g)

let appEndo (Endo f) = f

type All = All of bool with
    static member (?<-) (_    , _Monoid:Mempty , _:All  ) = All true
    static member (?<-) (All x, _Monoid:Mappend,   All y) = All (x && y)

type Any = Any of bool with
    static member (?<-) (_    , _Monoid:Mempty , _:Any  ) = Any false
    static member (?<-) (Any x, _Monoid:Mappend,   Any y) = Any (x || y)

type Sum<'a> = Sum of 'a with
    static member inline (?<-) (_         , _Monoid:Mempty , _:Sum<'n>   ) = Sum LanguagePrimitives.GenericZero :Sum<'n>
    static member inline (?<-) (Sum (x:'n), _Monoid:Mappend,   Sum (y:'n)) = Sum (x + y)                        :Sum<'n>

type Product<'a> = Product of 'a with
    static member inline (?<-) (_             , _Monoid:Mempty , _:Product<'n>   ) = Product LanguagePrimitives.GenericOne :Product<'n>
    static member inline (?<-) (Product (x:'n), _Monoid:Mappend,   Product (y:'n)) = Product (x * y)                       :Product<'n>