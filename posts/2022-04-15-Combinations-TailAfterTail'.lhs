---
title: Tail After Tail Story (Combinations In Haskell)
description: a haskell combination module (TailAfterTail.lhs)
keywords: haskell, combinations, benchmark
author: Myoungjin Jeon
tags: haskell, combinations, benchmark
---
[orig-tat]: https://jeongoon.github.io/posts/2022-04-03-Combinations-TailAfterTail.html
[benchmark-tat]: https://github.com/jeongoon/combinations-bench/blob/main/haskell-combinations/benchmarkTat/BenchTat.lhs

Copyright (c) 2022 JEON Myoungjin <jeongoon@g... >

LICENSE: [Open Software License 3.0](https://opensource.org/licenses/OSL-3.0)

== Combinations in Haskell Series

   1. [Combinations In Haskell (called Tail After Tail)][orig-tat]
   2. *Tail After Tail Story (Combinations In Haskell)*

= Same Theory; Different Implementation

In programming world, the pseudo code or theory will trigger your programming
implementation in many ways. This kind of diversion makes programming
interesting as well.

Previously I made [TailAfterTail.lhs][orig-tat]

And I found that [`inits`](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-List.html#v:inits)
or [`tails`](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-List.html#v:tails)
are not quite necessary if I don't rely on `scanl` or [`zipWith`](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-List.html#v:zipWith).

It is probably generally accepted that some different kind of implementation,
which consist of even small amount of codes, but which run at very high
frequency, will eventually show huge gaps in performance after many
iterations of execution.

So, this is about what I found during implementation of *Tail After Tail*
combinations.

== Module Begins

Firstly, I going to write down the all function will be exposed.
\begin{code}
{-# LANGUAGE BangPatterns #-}

module TailAfterTail
  ( combinationsWithScanl
  , combinationsWithSingleStep
  , combinations
  , combinationsWithTwoSteps
  , allCombinationsWithScanl
  , allCombinationsWithSingleStep
  , allCombinationsWithTwoSteps
  , allCombinations
  ) where

import Data.List (tails, inits, scanl') -- only required for **WithScanl
\end{code}

== Original Version (scanl)

Please Find out more information [HERE][orig-tat].

However, `combinations1'`, `flatten_allCombinationsGrouped` and
`genPart` will be common helper functions.

\begin{code}
combinations1' :: [a] -> [[[a]]]
combinations1' ms = [ [[m]] | m <- ms ]

flatten_allCombinationsGrouped allComboFunc = map concat . allComboFunc

genPart :: Foldable t => a -> t [[a]] -> [[a]]
genPart leader followerGroups = [ leader : followers
                                  | followers <- concat followerGroups ]

\end{code}

And I define some helper functions.

\begin{code}
usefulTails :: [a] -> [[a]]
usefulTails = init . tails

genStep :: [[[a]]] -> [a] -> [[[a]]]
genStep prevTails members' =
  zipWith genPart members' (usefulTails prevTails')
  where
    prevTails' = tail prevTails -- head is not useful

membersTails = reverse . tail . inits -- tail is used to skip empty list.

\end{code}

and finally `combinationsWithScanl` family goes below.

\begin{code}
allCombinationsWithScanl' :: [a] -> [[[[a]]]]
allCombinationsWithScanl' ms =
  scanl' genStep (combinations1' ms) (membersTails ms)

allCombinationsWithScanlGrouped :: [a] -> [[[a]]]
allCombinationsWithScanlGrouped =
  flatten_allCombinationsGrouped allCombinationsWithScanl'

allCombinationsWithScanl :: [a] -> [[a]]
allCombinationsWithScanl = concat . allCombinationsWithScanlGrouped
\end{code}

== Pure Implementation Without Scanl (SingleStep)

The following code is created without scanl or zipWith.

It gains slightly more performance with (bang pattern: !).
Which will be covered may be in another article.
But IMHO, it helps to reduce laziness and use less stack.

\begin{code}
unsafe_allCombinationsWithSingleStep :: [a] -> [[[[a]]]]
unsafe_allCombinationsWithSingleStep members =
  let
    helper ! cases = -- bang pattern added
      let
        genStep (m:ms) (_:cs:[]) = [ [ m : c | c <- cs ] ]
        genStep (m:ms) (_:cs) =
          -- note       ^ : we don't use first element
          genPart m cs : genStep ms cs
      in
         cases : helper (genStep members cases)
  in
    helper . combinations1' $ members

\end{code}

As you can see `helper` function is just an entry level wrapper function
and make a recursion call.

`genStep` will actually create *next* cases and act as thunk which is evaluated
later thanks to laziness in haskell.

I named the function as `unsafe_` on purpose. Because helper function actually
doesn't know when it will stop, and if you run `unsafe_allCombinationsWithSingleStep`
in bare context will explode with exception.

```sh
sh> ghci
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /your/home/.config/ghc/ghci.conf
λ> :l 2022-04-15-Combinations-TailAfterTail'.lhs
[1 of 1] Compiling TailAfterTail    ( 2022-04-15-Combinations-TailAfterTail'.lhs, interpreted )
Ok, one module loaded.
λ> unsafe_allCombinationsWithSingleStep [1..5]
[[[[1]],[[2]],[[3]],[[4]],[[5]]],[[[1,2],[1,3],[1,4],[1,5]],[[2,3],[2,4],[2,5]],[[3,4],[3,5]],[[4,5]]],[[[1,2,3],[1,2,4],[1,2,5],[1,3,4],[1,3,5],[1,4,5]],[[2,3,4],[2,3,5],[2,4,5]],[[3,4,5]]],[[[1,2,3,4],[1,2,3,5],[1,2,4,5],[1,3,4,5]],[[2,3,4,5]]],[[[1,2,3,4,5]]],[[]*** Exception: 2022-04-15-Combinations-TailAfterTail'.lhs:(120,9)-(122,52): Non-exhaustive patterns in function genStep
```

`unsafe_allCombinationsWithSingleStepGrouped` flatten the result but yet
grouped by selection size, this is still *unsafe* but [`combinationsWith`](#combinationsWith) will handle it.

\begin{code}
unsafe_allCombinationsWithSingleStepGrouped :: [a] -> [[[a]]]
unsafe_allCombinationsWithSingleStepGrouped =
  flatten_allCombinationsGrouped unsafe_allCombinationsWithSingleStep
\end{code}

So now we could get all combinations by flatten a more time.

\begin{code}
allCombinationsWithSingleStep :: [a] -> [[a]]
allCombinationsWithSingleStep members =
  concat
  --  this makes unsafe_* safe by limiting the size of list.
  . take (length members)
  . unsafe_allCombinationsWithSingleStepGrouped
  $ members


\end{code}

== With Two Steps

This is another version of without `scanl`. the Main improvement is that
this function separates the jobs into two operations:

- create **first cases** from the previous **tails**.
- create **rest of cases** and start next next case based on the result.

\begin{code}
allCombinationsWithTwoSteps' :: [a] -> [[[[a]]]]
allCombinationsWithTwoSteps'
  members@(fm:rms) = -- ^ fm : first member; rms: rest members
  let
    initFirstCase = [[fm]]
    initRestCases = combinations1' rms

    genFirstCases = genPart fm

    genRestCases _ [] = []
    genRestCases (m:ms) rcs@(_:rcs') = -- ^ rcs : rest of cases
      (genPart m $ rcs) : (genRestCases ms rcs')

\end{code}

It looks almost identical when comparing to `SingleStep` but
now `helper` function knows exactly where to start as `newTail` is
memorized at the moment. It only saves time to `tail` by pattern matching
in `SingleStep` but resuts are propagated when the choices are growing.

BTW, `tail` by pattern matching means (_:cs) in the following code.

```haskell
        genStep (m:ms) (_:cs) =
          -- note       ^ : we don't use first element
          [ m : c | c <- concat cs ] : genStep ms cs
```

And let's wire them up with initial case and *helper* function
\begin{code}
    helper [] = []
    helper ! prevTail =
      let
        newTail = genRestCases rms (tail prevTail)
      in
        ((genFirstCases prevTail) : newTail) : helper newTail
  in (initFirstCase : initRestCases) : helper initRestCases
\end{code}

the following steps are similar to the other implementation.

\begin{code}
allCombinationsWithTwoStepsGrouped :: [a] -> [[[a]]]
allCombinationsWithTwoStepsGrouped =
  flatten_allCombinationsGrouped allCombinationsWithTwoSteps'

allCombinationsWithTwoSteps :: [a] -> [[a]]
allCombinationsWithTwoSteps members =
  concat . allCombinationsWithTwoStepsGrouped $ members
\end{code}

Another benefit of the `TwoSteps` implementation is that we can stop
easily because now `newTail` is always available and we could know whether
next step is available or not. I don't need to name it *unsafe_* any more.

== combinations variant from each implementation

Now, it's time to make select `K` out of given choice.

And I found that this is a common helper function:

=== combinationsWith

\begin{code}
combinationsWith :: ([a] -> [[[a]]]) -> [a] -> Int -> Int -> [[a]]
combinationsWith allComboGroupedFunc ms n1@selectFrom n2@selectTo =
  let
    ( isFlipped, n1', n2' ) = -- smaller value first
      if n1 < n2 then ( False
                      , max n1 0
                      , max n2 0)
      else            ( True
                      , max n2 0
                      , max n1 0)
      -- and ensure all range value are zero or positive by usig `max`
    rangeLength = n2' - n1' + 1
    reverseIfNeeded
      | isFlipped = reverse
      | otherwise = id

  in
    -- note: read from the bottom
    concat                      -- 4. final flattening
    . reverseIfNeeded           -- 3. if user put opposite way, reverse it.
    . take rangeLength          -- 2. takes only interested lists
    . drop (pred n1')           -- 1. ignore some
    $ allComboGroupedFunc ms
\end{code}

And all variant combinations* function available as below:

\begin{code}
combinationsWithScanl      = combinationsWith allCombinationsWithScanlGrouped
combinationsWithSingleStep = combinationsWith unsafe_allCombinationsWithSingleStepGrouped
combinationsWithTwoSteps   = combinationsWith allCombinationsWithTwoStepsGrouped
\end{code}

== Benchmark
you can find the benchmark *code* on [my github repository][benchmark-tat].
To save your time, [THIS](https://github.com/jeongoon/combinations-bench/blob/main/haskell-combinations/benchmarkTat/result.out)
is one of my benchmark result.

== Choose Default *allCombinations* and *combinations*

After benchmarking, I found `AllcombinationsWithTwoSteps` shows best result
in all categories(small, medium, large) among them.

\begin{code}
allCombinations = allCombinationsWithTwoSteps
combinations = combinationsWithTwoSteps
\end{code}

