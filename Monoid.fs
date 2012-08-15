module Data.Monoid

open Prelude

type Mempty = Mempty with    
    static member        (?<-) (_Monoid:Mempty, _:List<'a> , _) = []      :List<'a>
    static member        (?<-) (_Monoid:Mempty, _:Maybe<'a>, _) = Nothing :Maybe<'a>
    static member        (?<-) (_Monoid:Mempty, _:array<'a>, _) = [||]    :array<'a>
    static member        (?<-) (_Monoid:Mempty, _:string   , _) = ""
    static member        (?<-) (_Monoid:Mempty, _:Ordering , _) = EQ
    static member        (?<-) (_Monoid:Mempty, _:unit     , _) = ()

let inline mempty() : ^R = Mempty ? (defaultof< ^R>) <- ()

type Mempty with static member inline (?<-)  (_Monoid:Mempty, _: 'a*'b         , _) =
                    (mempty(),mempty()                           ): 'a*'b
type Mempty with static member inline (?<-)  (_Monoid:Mempty, _: 'a*'b*'c      , _) =
                    (mempty(),mempty(),mempty()                  ): 'a*'b*'c
type Mempty with static member inline (?<-)  (_Monoid:Mempty, _: 'a*'b*'c*'d   , _) =
                    (mempty(),mempty(),mempty(),mempty()         ): 'a*'b*'c*'d
type Mempty with static member inline (?<-)  (_Monoid:Mempty, _: 'a*'b*'c*'d*'e, _) =
                    (mempty(),mempty(),mempty(),mempty(),mempty()): 'a*'b*'c*'d*'e


type Mappend = Mappend with        
    static member        (?<-) (_Monoid:Mappend, x:List<_>  , y      ) = x ++ y        
    static member        (?<-) (_Monoid:Mappend, x:array<_> , y      ) = x </Array.append/> y
    static member        (?<-) (_Monoid:Mappend, x:string   , y      ) = x + y
    static member        (?<-) (_Monoid:Mappend, x:Ordering , y      ) =
        match (x,y) with
        | (LT,_) -> LT
        | (EQ,a) -> a
        | (GT,_) -> GT
    static member        (?<-) (_Monoid:Mappend, (), _:unit ) = ()

let inline mappend (x:'a) (y:'a) : 'a = Mappend ? (x) <- y

type Mappend with
    static member inline (?<-) (_Monoid:Mappend, x:Maybe<_> , y      ) = 
        match (x,y) with
        | (Just a , Just b ) -> Just (a </mappend/> b)
        | (Just a , Nothing) -> Just a
        | (Nothing, Just b ) -> Just b
        | _                  -> Nothing


type Mappend with static member inline (?<-) (_Monoid:Mappend, (x1,x2         ), (y1,y2         )) = 
                    (mappend x1 y1,mappend x2 y2                                          ) :'a*'b
type Mappend with static member inline (?<-) (_Monoid:Mappend, (x1,x2,x3      ), (y1,y2,y3      )) =
                    (mappend x1 y1,mappend x2 y2,mappend x3 y3                            ) :'a*'b*'c
type Mappend with static member inline (?<-) (_Monoid:Mappend, (x1,x2,x3,x4   ), (y1,y2,y3,y4   )) =
                    (mappend x1 y1,mappend x2 y2,mappend x3 y3,mappend x4 y4              ) :'a*'b*'c*'d
type Mappend with static member inline (?<-) (_Monoid:Mappend, (x1,x2,x3,x4,x5), (y1,y2,y3,y4,y5)) =
                    (mappend x1 y1,mappend x2 y2,mappend x3 y3,mappend x4 y4,mappend x5 y5) :'a*'b*'c*'d*'e

let inline mconcat x =
    let foldR f s lst = List.foldBack f lst s
    foldR mappend (mempty()) x



type Dual<'a> = Dual of 'a with
    static member inline (?<-) (_Monoid:Mempty, _:Dual<'m>,      _) = Dual (mempty()) :Dual<'m>
    static member inline (?<-) (_Monoid:Mappend,  Dual x, Dual y  ) = Dual (y </mappend/> x)
let getDual (Dual x) = x

type Endo<'a> = Endo of ('a -> 'a) with
    static member        (?<-) (_Monoid:Mempty, _:Endo<'m>,      _) = Endo id  :Endo<'m>
    static member        (?<-) (_Monoid:Mappend,  Endo f, Endo g  ) = Endo (f << g)

let appEndo (Endo f) = f


type All = All of bool with
    static member (?<-) (_Monoid:Mempty, _:All  ,     _) = All true
    static member (?<-) (_Monoid:Mappend,  All x, All y) = All (x && y)

type Any = Any of bool with
    static member (?<-) (_Monoid:Mempty, _:Any  ,     _) = Any false
    static member (?<-) (_Monoid:Mappend,  Any x, Any y) = Any (x || y)

type Sum<'a> = Sum of 'a with
    static member inline (?<-) (_Monoid:Mempty, _:Sum<'n>   ,         _) = Sum 0G     :Sum<'n>
    static member inline (?<-) (_Monoid:Mappend,  Sum (x:'n), Sum(y:'n)) = Sum (x + y):Sum<'n>

type Product<'a> = Product of 'a with
    static member inline (?<-) (_Monoid:Mempty, _:Product<'n>   ,             _) = Product 1G     :Product<'n>
    static member inline (?<-) (_Monoid:Mappend,  Product (x:'n), Product(y:'n)) = Product (x * y):Product<'n>