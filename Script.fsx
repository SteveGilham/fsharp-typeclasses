// For F# 3.0 Preview use #nowarn "1173"


#load "Prelude.fs"
open Prelude


// return

let resSome2 :option<_> = return' 2
let resSing2 :list<_>   = return' 2


// List Monad

// F#                           // Haskell
let result = 
    do' {                       // do {
        let! x1 = [1;2]         //   x1 <- [1;2]
        let! x2 = [10;20]       //   x2 <- [10;20]
        return ((+) x1 x2) }    //   return ((+) x1 x2) }

// desugared version
let lst11n21n12n22 = [1;2]  >>= (fun x1 -> [10;20] >>= (fun x2 ->  return'((+) x1 x2 )))


// IO Monad

let action = do' {
    do! putStrLn  "What is your first name?"
    let! fn = getLine
    do! putStrLn  ("Thanks, " + fn) 
    do! putStrLn  ("What is your last name?")
    let! ln = getLine
    let  fullname = fn + " " + ln
    do! putStrLn  ("Your full name is: " + fullname)
    return fullname }
// try -> runIO action ;;


// Functors

let times2,plus3 = (*) 2, (+) 3

let valTimes3   = fmap plus3 (Some 4)
let noValue     = fmap plus3 None
let lstTimes2   = fmap times2 [1;2;3;4]
let times2plus3 = fmap times2 plus3
let getChars    = fmap (fun (x:string) -> x.ToCharArray() |> Seq.toList ) action
// try -> runIO getChars ;;



// Define a type Tree

type Tree<'a> =
    | Tree of 'a * Tree<'a> * Tree<'a>
    | Leaf of 'a
    static member map f (t:Tree< 'a>  )  =
        match t with
        | Leaf x -> Leaf (f x)
        | Tree(x,t1,t2) -> Tree(f x, Tree.map f t1, Tree.map f t2)

// add instance for Functor class
    static member (?<-) (_    , _Functor:Fmap, x:Tree<_>) = fun f -> Tree.map f x

let myTree = Tree(6, Tree(2, Leaf(1), Leaf(3)), Leaf(9))
let mappedTree = fmap times2plus3 myTree




// Monoids
#load "Monoid.fs"
open Data.Monoid

let emptyLst:list<int> = mempty()
let zeroInt:Sum<int>   = mempty()
let res10 = mappend (mempty()) (Sum 10)
let res6  = mconcat <| fmap Sum [0.4; 5.6]
let res8  = mconcat [mempty(); Sum 2; Sum 6]
let res8n4 = [mempty(); [8;4]]
let res15 = mappend (Product 15) (mempty()) 
let resTrue = mconcat [mempty(); Any true]
let resFalse = mconcat (fmap All [true;false])
let resHi = mappend (mempty()) "Hi"
let resGT = mappend (mempty()) GT
let resLT = mconcat [mempty(); LT ; EQ ;GT]
let res9823 = mconcat (fmap Dual [mempty();"3";"2";"8";"9"])
let resBA = mappend (Dual "A" ) (Dual "B" )
let resEl00:list<int>*Sum<float> = mempty()
let resS3P20    = mappend (Sum 1,Product 5.0) (Sum 2,Product 4.0)
let res230      = mappend (mempty(),mempty()) ([2],[3.0])
let res243      = mappend  ([2;4],[3]) (mempty())
let res23       = mappend (mempty()) ([2],"3")
let resLtDualGt = mappend  (LT,Dual GT) (mempty())


// Control Monad
#load "Monad.fs"
open Control.Monad.Base

let nameAndAddress = mapM (fun x -> putStrLn x >>= fun _ -> getLine) ["name";"address"]

let a:list<int> = mzero()
let res123      = mplus (mempty()) ([1;2;3])

// MonadPlus (sample code from http://en.wikibooks.org/wiki/Haskell/MonadPlus)
let pythags = do'{
  let! z = [1..50]
  let! x = [1..z]
  let! y = [x..z]
  do! guard (x*x + y*y = z*z)
  return (x, y, z)}

let pythags' = doPlus{
  let! z = [1..50]
  let! x = [1..z]
  let! y = [x..z]
  if (x*x + y*y = z*z) then return (x, y, z)}


// Arrows
#load "Arrow.fs"
open Control.Arrow

let r5:List<_>  = (runKleisli (id'())) 5
let f = Kleisli (fun y -> [y;y*2;y*3]) <<< Kleisli ( fun x -> [ x + 3 ; x * 2 ] )
let r8n16n24n10n20n30 = runKleisli f <| 5

let res3n6n9 = (arr (fun y -> [y;y*2;y*3])) 3
let resSome2n4n6:option<_> = runKleisli (arr (fun y -> [y;y*2;y*3])) 2

let res500n19 = ( (*) 100) *** ((+) 9)   <| (5,10)
let res500n14 = ( (*) 100) &&& ((+) 9)   <| 5


// Applicative functors

#load "Applicative.fs"
open Control.Applicative

// lists
let res3n4 = pure' ((+) 2) <*> [1;2]

// functions
let res3 = pure' 3 "anything"
let res607 = fmap (+) ( (*) 100 ) 6 7
let res606 = ( (+) <*>  (*) 100 ) 6
let res508 = (fmap (+) ((+) 3 ) <*> (*) 100) 5

//ZipList
let res9n5   = fmap ((+) 1) (ZipList(seq [8;4]))
let res18n24 = pure' (+) <*> ZipList(seq [8;4]) <*> ZipList(seq [10;20])


// Foldable
#load "Foldable.fs"

open Data.Foldable

let resGt = foldMap (compare' 2) [1;2;3]
let resHW = foldMap (fun x -> Some ("hello " + x)) (Some "world")

module FoldableTree =
    type Tree<'a> =
        | Empty 
        | Leaf of 'a 
        | Node of (Tree<'a>) * 'a * (Tree<'a>)

        // add instance for Foldable class
        static member inline ($)   (FoldMap, t:Tree<_> ) =
            let rec _foldMap x f =
                match x with
                | Empty        -> mempty()
                | Leaf n       -> f n
                | Node (l,k,r) -> mappend (_foldMap l f) (mappend (f k) (_foldMap r f) )
            _foldMap t
        static member inline (?<-) (_, Foldr, x:Tree<_>  ) = fun (f,z) -> Foldr.foldr f z x
    
    let myTree = Node (Node (Leaf(1), 6, Leaf(3)), 2 , Leaf(9))
    let resSum21      = foldMap Sum     myTree
    let resProduct324 = foldMap Product myTree
    let res21         = foldr   (+) 0   myTree




// Traversable
#load "Traversable.fs"

open Data.Traversable

let f x = if x < 200 then [3 - x] else []
let g x = if x < 200 then Some (3 - x) else None

let resSomeminus100 = traverse f (Some 103)
let resLstOfNull    = traverse f None 
let res210          = traverse f [1;2;3]  
let resSome210      = traverse g [1;2;3]  
let resEmptyList    = traverse f [1000;2000;3000] 
let resEListOfElist = traverse f []
let resSome321  = sequenceA [Some 3;Some 2;Some 1]
let resNone     = sequenceA [Some 3;None  ;Some 1]
let res654      = sequenceA [ (+)3 ; (+)2 ; (+) 1] 3
let resCombined = sequenceA [ [1;2;3] ; [4;5;6]  ]
let get3strings = sequenceA [getLine;getLine;getLine]

#load "Cont.fs"
open Control.Monad.Cont

#load "State.fs"
open Control.Monad.State

// from http://www.haskell.org/haskellwiki/State_Monad
let x1 = runState (return' 'X') 1
let xf:State<int,_> = return' 'X'
let r11    = runState get 1
let rUnit5 = runState (put 5) 1
let rX5    = runState (do' { 
    do! put 5
    return 'X' }) 1
let postincrement = do' {
    let! x = get
    do! put (x+1)
    return x }
let r12 = runState postincrement 1

let tick :State<_,_> = do'{
    let! n = get
    do! put (n+1)
    return n}

let plusOne n = execState tick n
let plus  n x = execState (sequence <| List.replicate n tick) x


#load "Writer.fs"
open Control.Monad.Writer


#load "MonadTrans.fs"
open Control.Monad.Trans
open Control.Monad.Trans.MaybeT
open Control.Monad.Trans.ListT

let maybeT = MaybeT [Some 2; Some 4] >>= fun x -> MaybeT [Some x; Some (x+10)]
let listT  = ListT  (Some [2;4]    ) >>= fun x -> ListT  (Some [x; x+10]     )

let apMaybeT = ap (MaybeT [Some ((+) 3)] ) ( MaybeT [Some  3 ] )
let apListT  = ap (ListT  (Some [(+) 3]) ) ( ListT  (Some [3]) )

let getAtLeast8Chars:MaybeT<_> =  lift getLine >>= fun s -> (guard (String.length s >= 8) ) >>= fun _ -> return' s
//try -> runIO <| runMaybeT getAtLeast8Chars