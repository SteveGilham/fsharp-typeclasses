namespace InlineAbstractions.TypeClasses

open InlineAbstractions.Prelude
open InlineAbstractions.TypeClasses.Functor
open InlineAbstractions.TypeClasses.Applicative
open InlineAbstractions.TypeClasses.Foldable

module Traversable = 
    type Traverse = Traverse with
        static member inline instance (Traverse, t:option<_>, _) = fun f -> match t with Some x -> fmap Some (f x) | _ -> pure' None    
        static member inline instance (Traverse, t:List<_>  , _) = fun f ->            
            let cons_f x ys = fmap cons (f x) <*> ys
            foldr cons_f (pure' []) t

    let inline internal traverse f t = Inline.instance (Traverse, t) f
    let inline internal sequenceA  x = traverse id x