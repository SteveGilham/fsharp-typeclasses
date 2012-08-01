module Data.Monoid

open Prelude

type Mempty = Mempty with    
    static member        (?<-) (_, _Monoid:Mempty, _:List<'a> ) = []      :List<'a>
    static member        (?<-) (_, _Monoid:Mempty, _:Maybe<'a>) = Nothing :Maybe<'a>
    static member        (?<-) (_, _Monoid:Mempty, _:array<'a>) = [||]    :array<'a>
    static member        (?<-) (_, _Monoid:Mempty, _:string   ) = ""
    static member        (?<-) (_, _Monoid:Mempty, _:Ordering ) = EQ
    static member        (?<-) (_, _Monoid:Mempty, _:unit     ) = ()

let inline mempty() : ^R = () ? (Mempty) <- defaultof< ^R>

type Mempty with static member inline (?<-)  (_, _Monoid:Mempty, _: 'a*'b         ) =
                    (mempty(),mempty()                           ): 'a*'b
type Mempty with static member inline (?<-)  (_, _Monoid:Mempty, _: 'a*'b*'c      ) =
                    (mempty(),mempty(),mempty()                  ): 'a*'b*'c
type Mempty with static member inline (?<-)  (_, _Monoid:Mempty, _: 'a*'b*'c*'d   ) =
                    (mempty(),mempty(),mempty(),mempty()         ): 'a*'b*'c*'d
type Mempty with static member inline (?<-)  (_, _Monoid:Mempty, _: 'a*'b*'c*'d*'e) =
                    (mempty(),mempty(),mempty(),mempty(),mempty()): 'a*'b*'c*'d*'e


type Mappend = Mappend with        
    static member        (?<-) (x:List<_>  , _Monoid:Mappend, y      ) = x ++ y        
    static member        (?<-) (x:array<_> , _Monoid:Mappend, y      ) = x </Array.append/> y
    static member        (?<-) (x:string   , _Monoid:Mappend, y      ) = x + y
    static member        (?<-) (x:Ordering , _Monoid:Mappend, y      ) =
        match (x,y) with
        | (LT,_) -> LT
        | (EQ,a) -> a
        | (GT,_) -> GT
    static member        (?<-) (()         , _Monoid:Mappend, _:unit ) = ()

let inline mappend (x:'a) (y:'a) : 'a = x ? (Mappend) <- y

type Mappend with
    static member inline (?<-) (x:Maybe<_> , _Monoid:Mappend, y      ) = 
        match (x,y) with
        | (Just a , Just b ) -> Just (a </mappend/> b)
        | (Just a , Nothing) -> Just a
        | (Nothing, Just b ) -> Just b
        | _                  -> Nothing


type Mappend with static member inline (?<-) ((x1,x2         ), _Monoid:Mappend, (y1,y2         )) = 
                    (mappend x1 y1,mappend x2 y2                                          ) :'a*'b
type Mappend with static member inline (?<-) ((x1,x2,x3      ), _Monoid:Mappend, (y1,y2,y3      )) =
                    (mappend x1 y1,mappend x2 y2,mappend x3 y3                            ) :'a*'b*'c
type Mappend with static member inline (?<-) ((x1,x2,x3,x4   ), _Monoid:Mappend, (y1,y2,y3,y4   )) =
                    (mappend x1 y1,mappend x2 y2,mappend x3 y3,mappend x4 y4              ) :'a*'b*'c*'d
type Mappend with static member inline (?<-) ((x1,x2,x3,x4,x5), _Monoid:Mappend, (y1,y2,y3,y4,y5)) =
                    (mappend x1 y1,mappend x2 y2,mappend x3 y3,mappend x4 y4,mappend x5 y5) :'a*'b*'c*'d*'e

let inline mconcat x =
    let foldR f s lst = List.foldBack f lst s
    foldR mappend (mempty()) x



type Dual<'a> = Dual of 'a with
    static member inline (?<-) (_     , _Monoid:Mempty , _:Dual<'m>) = Dual (mempty()) :Dual<'m>
    static member inline (?<-) (Dual x, _Monoid:Mappend,   Dual y  ) = Dual (y </mappend/> x)
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
    static member inline (?<-) (_         , _Monoid:Mempty , _:Sum<'n>  ) = Sum 0G     :Sum<'n>
    static member inline (?<-) (Sum (x:'n), _Monoid:Mappend,   Sum(y:'n)) = Sum (x + y):Sum<'n>

type Product<'a> = Product of 'a with
    static member inline (?<-) (_             , _Monoid:Mempty , _:Product<'n>  ) = Product 1G     :Product<'n>
    static member inline (?<-) (Product (x:'n), _Monoid:Mappend,   Product(y:'n)) = Product (x * y):Product<'n>