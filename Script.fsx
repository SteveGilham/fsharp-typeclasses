#load "Prelude.fs"
open Prelude


// return

let anOption :option<_> = return' 2
let aList    :list<_>   = return' 2


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
    let! _  = putStrLn  "What is your first name?"
    let! fn = getLine
    let! _  = putStrLn  ("Thanks, " + fn) 
    let! _  = putStrLn  ("What is your last name?")
    let! ln = getLine
    let  fullname = fn + " " + ln
    let! _  = putStrLn  ("Your full name is: " + fullname)
    return fullname }
// try -> IO.Invoke action ;;


// Functors

let times2,plus3 = (*) 2, (+) 3

let valTimes3   = fmap plus3 (Some 4)
let noValue     = fmap plus3 None
let lstTimes2   = fmap times2 [1;2;3;4]
let times2plus3 = fmap times2 plus3
let getChars    = fmap (fun (x:string) -> x.ToCharArray() |> Seq.toList ) action
// try -> IO.Invoke getChars ;;



// Define a type Tree

type Tree<'a> =
    | Tree of 'a * Tree<'a> * Tree<'a>
    | Leaf of 'a
    static member map f (t:Tree< 'a>  )  =
        match t with
        | Leaf x -> Leaf (f x)
        | Tree(x,t1,t2) -> Tree(f x, Tree.map f t1,  Tree.map f t2)

// add instance for Functor class
    static member (?) (x:Tree<_>,cs:Fmap )   = fun f -> Tree.map    f x

let myTree = Tree(6, Tree(2, Leaf(1), Leaf(3)), Leaf(9))
let mappedTree = fmap times2plus3 myTree



// Applicative functors

#load "Monad.fs"
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
let res18n24 = pure' (+) <*> ZipList(seq [8;4]) <*> ZipList(seq [10;20])


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
let resGT = mappend (mempty()) LT
let resEQ = mconcat [mempty(); LT ; EQ ;GT]
let res9823 = mconcat (fmap Dual [mempty();"3";"2";"8";"9"])
let resEl00:list<int>*Sum<float> = mempty()
let resS3P20    = mappend (Sum 1,Product 5.0) (Sum 2,Product 4.0)
let res230      = mappend (mempty(),mempty()) ([2],[3.0])
let res243      = mappend  ([2;4],[3]) (mempty())
let res23       = mappend (mempty()) ([2],"3")
let resLtDualLt = mappend  (LT,Dual GT) (mempty())
let resDualSum2Eq35     = mappend (mempty(),[3;5]) (Dual (Sum 2,EQ),mempty())
let resGT234DualSum33   = mconcat [(EQ,([2],Dual (Sum 3))) ; (LT,([3;4],Dual (Sum 30))) ]