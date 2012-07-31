module Data.Monoid

open Prelude

#nowarn "64"

type Monoid = Monoid with    
    static member        mempty (Monoid, _:'a list  ) = []    :'a list
    static member        mempty (Monoid, _:'a option) = None  :'a option
    static member        mempty (Monoid, _:'a[]     ) = [||]  :'a[]
    static member        mempty (Monoid, _:string   ) = ""
    static member        mempty (Monoid, _:Ordering ) = EQ
    static member        mempty (Monoid, _:unit     ) = ()

let inline mempty() : ^R = ((^C or ^R) : (static member mempty  : ^C * ^R -> _) (Monoid, defaultof< ^R>))

type Monoid with static member inline mempty  (Monoid, _: 'a*'b         ) =
                    (mempty(),mempty()                           ): 'a*'b
type Monoid with static member inline mempty  (Monoid, _: 'a*'b*'c      ) =
                    (mempty(),mempty(),mempty()                  ): 'a*'b*'c
type Monoid with static member inline mempty  (Monoid, _: 'a*'b*'c*'d   ) =
                    (mempty(),mempty(),mempty(),mempty()         ): 'a*'b*'c*'d
type Monoid with static member inline mempty  (Monoid, _: 'a*'b*'c*'d*'e) =
                    (mempty(),mempty(),mempty(),mempty(),mempty()): 'a*'b*'c*'d*'e


let inline mappend (x:^a) (y:^a) : 'a = ((^C or ^a) : (static member mappend: ^C * ^a * ^a -> _) (Monoid, x, y))
        
type Monoid with        
    static member        mappend (Monoid,x:list<_>  , y      ) = List.append  x y        
    static member inline mappend (Monoid,x:option<_>, y      ) = 
        match (x,y) with 
        | (Some a,Some b) -> Some(mappend a b)
        | (Some a,None  ) -> Some a
        | (None  ,Some b) -> Some b
        | _               -> None
    static member        mappend (Monoid, x:_[]      , y      ) = Array.append x y
    static member        mappend (Monoid, x:string   , y      ) = x + y
    static member        mappend (Monoid, x:Ordering , y      ) =
        match (x,y) with
        | (LT,_) -> LT
        | (EQ,a) -> a
        | (GT,_) -> GT
    static member        mappend (Monoid, (), _:unit ) = ()



type Monoid with static member inline mappend (Monoid, (x1,x2         ), (y1,y2         )) = 
                    (mappend x1 y1,mappend x2 y2                                          ) :'a*'b
type Monoid with static member inline mappend (Monoid, (x1,x2,x3      ), (y1,y2,y3      )) =
                    (mappend x1 y1,mappend x2 y2,mappend x3 y3                            ) :'a*'b*'c
type Monoid with static member inline mappend (Monoid, (x1,x2,x3,x4   ), (y1,y2,y3,y4   )) =
                    (mappend x1 y1,mappend x2 y2,mappend x3 y3,mappend x4 y4              ) :'a*'b*'c*'d
type Monoid with static member inline mappend (Monoid, (x1,x2,x3,x4,x5), (y1,y2,y3,y4,y5)) =
                    (mappend x1 y1,mappend x2 y2,mappend x3 y3,mappend x4 y4,mappend x5 y5) :'a*'b*'c*'d*'e

let inline mconcat x =
    let foldR f s lst = List.foldBack f lst s
    foldR mappend (mempty()) x



type Dual<'a> = Dual of 'a with
    static member inline mempty  (Monoid,       _:Dual<'m>) = Dual (mempty()   ) :Dual<'m>
    static member inline mappend (Monoid, Dual x, Dual y  ) = Dual (mappend y x)
let getDual (Dual x) = x

type Endo<'a> = Endo of ('a -> 'a) with
    static member        mempty  (Monoid, _:Endo<'m>) = Endo id  :Endo<'m>
    static member        mappend (Monoid,   Endo f, Endo g  ) = Endo (f << g)

let appEndo (Endo f) = f

open LanguagePrimitives

type All = All of bool with
    static member mempty  (Monoid, _:All  ) = All true
    static member mappend (Monoid,   All x, All y) = All (x && y)

type Any = Any of bool with
    static member mempty  (Monoid, _:Any  ) = Any false
    static member mappend (Monoid,   Any x, Any y) = Any (x || y)

type Sum<'a> = Sum of 'a with
    static member inline mempty  (Monoid, _:Sum<'n>  ) = Sum GenericZero :Sum<'n>
    static member inline mappend (Monoid,   Sum (x:'n), Sum(y:'n)) = Sum (x + y)     :Sum<'n>

type Product<'a> = Product of 'a with
    static member inline mempty  (Monoid, _:Product<'n>  ) = Product GenericOne :Product<'n>
    static member inline mappend (Monoid,   Product (x:'n), Product(y:'n)) = Product (x * y)    :Product<'n>