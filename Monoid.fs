module Data.Monoid

open Prelude

type Mempty = Mempty with   
    static member        instance (_Monoid:Mempty, _:List<'a>  ) = fun () -> []   :  List<'a>
    static member        instance (_Monoid:Mempty, _:option<'a>) = fun () -> None :option<'a>
    static member        instance (_Monoid:Mempty, _:array<'a> ) = fun () -> [||] : array<'a>
    static member        instance (_Monoid:Mempty, _:string    ) = fun () -> ""
    static member        instance (_Monoid:Mempty, _:unit      ) = fun () -> ()

let inline mempty() = Inline.instance Mempty ()

type Mempty with static member inline instance (_Monoid:Mempty, _ : 'a*'b         ) = fun () ->
                    (mempty(),mempty()                           ): 'a*'b
type Mempty with static member inline instance (_Monoid:Mempty, _ : 'a*'b*'c      ) = fun () ->
                    (mempty(),mempty(),mempty()                  ): 'a*'b*'c
type Mempty with static member inline instance (_Monoid:Mempty, _ : 'a*'b*'c*'d   ) = fun () ->
                    (mempty(),mempty(),mempty(),mempty()         ): 'a*'b*'c*'d
type Mempty with static member inline instance (_Monoid:Mempty, _ : 'a*'b*'c*'d*'e) = fun () ->
                    (mempty(),mempty(),mempty(),mempty(),mempty()): 'a*'b*'c*'d*'e


type Mappend = Mappend with       
    static member        instance (_Monoid:Mappend, x:List<_>  , _) = fun y -> x @ y       
    static member        instance (_Monoid:Mappend, x:array<_> , _) = fun y -> Array.append x y
    static member        instance (_Monoid:Mappend, x:string   , _) = fun y -> x + y
    static member        instance (_Monoid:Mappend, ()         , _) = fun () -> ()

let inline mappend (x:'a) (y:'a) :'a = Inline.instance (Mappend, x) y

type Mappend with
    static member inline instance (_Monoid:Mappend, x:option<_> , _) = fun y ->
        match (x,y) with
        | (Some a , Some b) -> Some (mappend a b)
        | (Some a , None  ) -> Some a
        | (None   , Some b) -> Some b
        | _                 -> None


type Mappend with static member inline instance (_Monoid:Mappend, (x1,x2         ), _) = fun (y1,y2         ) ->
                    (mappend x1 y1,mappend x2 y2                                          ) :'a*'b
type Mappend with static member inline instance (_Monoid:Mappend, (x1,x2,x3      ), _) = fun (y1,y2,y3      ) ->
                    (mappend x1 y1,mappend x2 y2,mappend x3 y3                            ) :'a*'b*'c
type Mappend with static member inline instance (_Monoid:Mappend, (x1,x2,x3,x4   ), _) = fun (y1,y2,y3,y4   ) ->
                    (mappend x1 y1,mappend x2 y2,mappend x3 y3,mappend x4 y4              ) :'a*'b*'c*'d
type Mappend with static member inline instance (_Monoid:Mappend, (x1,x2,x3,x4,x5), _) = fun (y1,y2,y3,y4,y5) ->
                    (mappend x1 y1,mappend x2 y2,mappend x3 y3,mappend x4 y4,mappend x5 y5) :'a*'b*'c*'d*'e

let inline mconcat x =
    let foldR f s lst = List.foldBack f lst s
    foldR mappend (mempty()) x


type Dual<'a> = Dual of 'a with
    static member inline instance (_Monoid:Mempty , _:Dual<'m>   ) = fun () -> Dual (mempty()) :Dual<'m>
    static member inline instance (_Monoid:Mappend,   Dual x  , _) = fun (Dual y) -> Dual (y </mappend/> x)
let getDual (Dual x) = x

type Endo<'a> = Endo of ('a -> 'a) with
    static member        instance (_Monoid:Mempty , _:Endo<'m>   ) = fun () -> Endo id  :Endo<'m>
    static member        instance (_Monoid:Mappend,   Endo f  , _) = fun (Endo g) -> Endo (f << g)

let appEndo (Endo f) = f


type All = All of bool with
    static member instance (_Monoid:Mempty, _:All     ) = fun () -> All true
    static member instance (_Monoid:Mappend,  All x, _) = fun (All y) -> All (x && y)

type Any = Any of bool with
    static member instance (_Monoid:Mempty, _:Any     ) = fun () -> Any false
    static member instance (_Monoid:Mappend,  Any x, _) = fun (Any y) -> Any (x || y)

type Sum<'a> = Sum of 'a with
    static member inline instance (_Monoid:Mempty, _:Sum<'n>      ) = fun ()          -> Sum 0G     :Sum<'n>
    static member inline instance (_Monoid:Mappend,  Sum (x:'n), _) = fun (Sum(y:'n)) -> Sum (x + y):Sum<'n>

type Product<'a> = Product of 'a with
    static member inline instance (_Monoid:Mempty, _:Product<'n>      ) = fun ()              -> Product 1G     :Product<'n>
    static member inline instance (_Monoid:Mappend,  Product (x:'n), _) = fun (Product(y:'n)) -> Product (x * y):Product<'n>