# About fsharp-typeclasses

This is a convenience/archival export of a library from GoogleCode.  The active successor project to this one is gusty's [FSharpPlus (F#+)](https://github.com/gusty/FSharpPlus) here on GitHub.

## What license applies to this code

[Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0)

## Basic How-To

The original ReadMe.txt reads as follows

> To open this project:
> 
> 1. Open and Compile /InlineHelper/InlineHelper.fsproj with F# 3.0 then copy InlineHelper.dll to /lib/ : Alternatively or if you don't have F# 3.0 you can download InlineHelper.dll from http://fsharp-typeclasses.googlecode.com/files/fsharp-typeclasses-r118.zip
> 
> 2. Open the project file fsharp-typeclasses.fsproj
>
> 3. Have a look at Scripts.fsx for sample code.

*Note:* The link to Google Code no longer functions, but then you should be using a more recent F# these days, anyway.

# Overview

Emulate Haskell's typeclasses in F#

Demonstrate how using inline and operator overloading is possible to emulate Haskell's typeclasses in F#.

This project is based on this post http://nut-cracker.azurewebsites.net/index.php/typeclasses-for-fsharp

There is a project based on this approach https://github.com/gmpl/FsControl more specific to F# in general (and less tied to Haskell).
Sample code

```

    fmap ((+) 2) [1;2;3] ;; val it : int list = [3; 4; 5] fmap ((+) 2) (Some 3) ;; val it : int option = Some 5

    let a:list<_> = return' 1 ;; val a : int list = [1]

    let a:option<_> = return' 1 ;; val a : int option = Some 1

    [(+) 10;(*) 2] >>= fun f -> [f 1;f 2;f 3] ;; val it : int list = [11; 12; 13; 2; 4; 6]

    return' ((+) 10) >>= fun f -> Some (f 1) ;; val it : int option = Some 11

    // F# // Haskell let result = do' { // do { let! x1 = [1;2] // x1 <- [1;2] let! x2 = [10;20] // x2 <- [10;20] return ((+) x1 x2) } // return ((+) x1 x2) } ;;

val result : int list = [11; 21; 12; 22]

```

## Advantages (compared to other approaches)

* Type-safe: no cast, unbox required.

* Performance: no reflection or any kind of run-time mechanism involved. Everything happens at compile time.

* Although declaring Typeclasses could be sometimes tricky, to use them is very straight forward and there is no code overhead when using existing type classes. It' s easy to run Haskell snippets with minimum syntax translation.

* It's possible to implement Typeclasses which in Haskell involves Higher Kinds like Functor, Monad, Category and Arrow.

* No need to recompile the code to add instances of a new type.

## Limitations:

* As this technique involves inline with static constraints, overloaded functions can be called only from F#.

* To write Typeclasses using an intermediate DU with the ternary operator could be tricky for some cases and they should include at least two instances in order to make type inference guess the proper statically constrained type.

* There is no real group of functions, they could work sometimes individually. Nothing prevents you from using an incomplete definition of the Typeclass. As an example you can have return' defined but not (>>=) and calling to return' would still work. In some cases this is considered more as a feature than a limitation.

* Constraints of undecided types are not very readable.

* Orphaned instances are not supported. Instances must be implemented either in the class or in the type. I'm not sure if it is possible or not to support orphaned instances but even in Haskell they are not a good practice.

* ~~Default implementations are not supported, you have to explicitly specify which implementation you want for each function.~~ There is a way to implement defaults, it was implemented in [FsControl](https://github.com/gmpl/FsControl)

* Compile time could be long depending on how the library is used. Here are some advices to improve compile time:

    . Use the typeclass library as a compiled dll

    . Avoid do notation when not really needed.

    . Specifying type annotations will help type inference and thus reduce compile time.

## Translating from Haskell

| Haskell | F# | |:------------|:-----------| | functions | | |const | const' | |return | return' | |pure | pure' | |show | string | | Polymorphic constants | | |mempty | mempty() | |mzero | mzero() | |id (arrows) | id'() | | Operators | | | $ | <| | | . | << | | . (arrows) | <<< | | :: | : | | /= | =/ | | | <<|> | | ^ | **^ | | ^^ | **^^ | | Literals | | | 5 | 5G | | "hello world" | !"hello world" | | func | | | Do notation | | | do | do' {...}| | x <- a | let! x = a| | f x | do! f x | | Do notation (last line) | | | return a | return a | | a | return! a|

## *_NOTES_*

To improve readability I adopted some conventions:

Type Classes are created by defining one type for each member.

The type will be defined as a "singleton" Discriminated Union (DU) where the name is the same as the function but first letter is uppercase.

The operator will always be the ternary operator (?<-) used for dynamic assignment.

1st parameter: it's always the DU and will have the name of the Typeclass starting with an underscore.

Input parameter: if used will be the 2nd. An input parameter of a polymorphic type.

Output parameter: In this case the parameter name will be an underscore and the return type specified.

If we didn't use an input parameter, we'll have to fill the 3rd parameter with an unused one.

To summarize, we can have two different cases: 
1.  `static member [inline] (?<-) (TypeClassName:MemberName, _:ReturnType , _ )`
2.  `static member [inline] (?<-) (TypeClassName:MemberName, x:InputParam, _:ReturnType)`

### Update

Since [revision 117](http://code.google.com/p/fsharp-typeclasses/source/detail?r=bfb011cdbd668c42a676b5d2f06a80fc8e6900d0) the convention using the (?<-) operator was discarded. Now the convention is to use a method named "instance" and specify all parameters you need in order, like this:

`static member [inline] instance (TypeClassName:MemberName, p1:InputParam1, ... , pn:InputParamN, _:ReturnType )`

## Alternative Solution

UPDATE: This is no longer needed. The bug in the new F# version was fixed and I was able to compile a function that replace and extend the old use of the (?<-) operator.

One frequent question is why do we need to use the ternary operator (?<-).

Can't we use just statics methods?

The answer is in theory yes, but there ~~is~~ was a bug in the F# parser that fails to parse more than 2 'or' expressions in the call to a method with a static constraint.

This may sound confusing, so here are two examples: 

`let inline return' x : ^R = ((^C or ^R) : (static member return' : ^C * ^R -> _) (Monad, defaultof< ^R>) ) x`

` let inline (>>=) x f : ^R = ((^C or ^a or ^R) : (static member bind : ^C * ^a * ^R -> _) (Monad, x, defaultof< ^R>)) f`

The first line will work, because it involves only two terms inside the 'or' expression but the second line will fail because it involves 3.

The [alternative solution](https://github.com/SteveGilham/fsharp-typeclasses.staticmethods) shows how the code will looks like if that bug is fixed, and it includes a fix for the F# compiler, so we can recompile F# and use it to compile the alternative solution.

So that's why in the default version we use (?<-), because with operators the 'or' expression is inferred, and there is no need to put it explicitly which will fail to parse.
