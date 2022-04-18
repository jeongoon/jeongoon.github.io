---
title: Combinations In Haskell (called Tail After Tail)
description: another haskell combination module (TailAfterTail.lhs)
keywords: haskell, combinations, raku
author: Myoungjin Jeon
---
[leaders-and-followers]: https://jeongoon.github.io/posts/2022-03-29-Combination.html

Copyright (c) 2022 JEON Myoungjin <jeongoon@g... >

LICENSE: [Open Software License 3.0](https://opensource.org/licenses/OSL-3.0)

== Combinations in Haskell Series

   1. [Combinations In Haskell (called Leaders and Followers)][leaders-and-followers]
   2. *Combinations In Haskell (called Tail After Tail)*
   3. [Tail After Tail Story (Combinations In Haskell)](/posts/2022-04-15-Combinations-TailAfterTail'.html)

= Yet Another Combinations

I was trying to make a new type of combinations after making a combinations
method in `golang`, which is fast as expected as usual. It's impressive how
golang shows splend performance even though I didn't tweak the code much.

And my first haskell `resursive` version of combinations ([LeadersAndFollowers](/posts/2022-03-29-Combination.html))
isn't quite fast enough even though most of all concept behind the code is
creative, but it is not a single, natural flow of generating a list.
So I wrote down the pattern and found something I could apply.

= Tail After Tail

I named the module *Tail After Tail* which will be explained later.
Two different function `allCombinations`,  `combinations` are exported.

\begin{code}
module TailAfterTail
  ( combinations
  , allCombinations
  ) where

import Data.List (inits, tails)
\end{code}

= Find Out The Pattern

Let's say given selection list is [1,2,3,4,5]
```haskell
λ> members = [1..5]
```

And we will select some from the `members`.

== Select Only One

**Note:** `N` stands for number of selection.

This is too easy case. it will look like by `list comprehension`.
```haskell
combinations1 ms = [ [m] | m <- ms ]
```

Or by using `map`.

```haskell
combinations1 ms = map (\m -> [m]) ms
```

Or in more pointer-free way, which I don't highly recommend.
but sometimes very handy when combining with `.` operator.

```haskell
combinations1 = map (:[])
```

I found that *list comprehension* is most readable form here, so I will use
it more often.

== Select Two

I'm going to start from left always and `1` will be fist *part* of combinations.

Let's remind *N=1 case* as following

```sh
λ> members = [1..5]
λ> combinations1 members
[[1],[2],[3],[4],[5]]
```

=== Part1 (N=2, P=1)

Let's have a look into first part of *(N=2) case*

```haskell
λ> combinations2part1 [ 1 : [m] | m <- [2..5] ]
-- or
λ> combinations2part1 ms = [1 : [m] | m <- (tail [1..5]) ]

-- which is
λ> combinations2part1 ms = [1 : [m] | m <- (tail $ combinations1 ms) ]
```

How about Part 2 ??

=== Part2 (N=2, P=2)

```haskell
λ> combinations2part2 ms = [2 : [t] | t <- [3..5] ]
λ> -- or
λ> combinations2part2 ms = [2 : [t] | t <- (tail . tail $ combinations1 ms)]
```

As we are going to next part (1 -> 2 -> 3..), rest of cases (*[m]*) are reducing
by `tail` of original *members*.

Let's summarise what we found:

`P` stands for partition of generating.

```haskell
{ N=1 }
  [[1],[2],[3],[4],[5]]
       ^^^^^^^^^^^^^^^ -> tailN1_1
           ^^^^^^^^^^^ -> tailN1_2
               ^^^^^^^ -> tailN1_3
{ N=1, Part = 2 } : N/A

----------------------------------------------------------------

{ N=2, P=1 }
  [ 1 : [t] | t <- tailN1_1 ] => [[1,2],[1,3],[1,4],[1,5]]
{ N=2, P=2 }
  [ 2 : [t] | t <- tailN1_2 ] => [[2,3],[2,4],[2,5]]
{ N=2, P=3 }
  [ 3 : [t] | t <- tailN1_3 ] => [[3,4],[3,5]]
{ N=2, P=4 }
  [ 4 : [t] | t <- tailN1_4 ] => [[4,5]]
{ N=2, P=5 }
  N/A as tailN1P1_5 == []

In total, accumlated results are below.

{ N=2 } -- without flattening
  [[[1,2],[1,3],[1,4],[1,5]],[[2,3],[2,4],[2,5]],[[3,4],[3,5]],[[4,5]]]
```

**Note:** we need to flatten the partial results by calling `concat` later
when we need to do it.

And in next step I found more similarity in generating the combinations
with subtle difference.

== Select Three

```haskell

-- again __without__ flattening
{ N=2 }
   vvvvvvvvvvvvvvvvvvvvvvvvv -> not useful for next
  [[[1,2],[1,3],[1,4],[1,5]],[[2,3],[2,4],[2,5]],[[3,4],[3,5]],[[4,5]]]
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^->tailN2_1
                                                 ^^^^^^^^^^^^^^^^^^^^^->tailN2_2

-- but we need to flatten the list before combining.
{ N=3, P=1 }
  [ 1 : [t] | t <- (concat tailN2_1) ]
  => [[1,2,3],[1,2,4],[1,2,5],[1,3,4] .. ]

{ N=3, P=2 }
  [ 2 : [t] | t <- (concat tailN2_2) ]
  => [[2,3,4],[2,3,5],[2,4,5]]

  ..
{ N=3, P=4 }
  N/A as tailN2_4 is empty (== [])
```

And final (N=3) will forms like below:

```haskell
{ N=3 } -- without flattening
[[[1,2,3],[1,2,4],[1,2,5],[1,3,4],[1,3,5],[1,4,5]],
 [[2,3,4],[2,3,5],[2,4,5],
 [[3,4,5]
]
```

To make the same function for creating list of the part, we need to
redefine {N=1} cases.

```haskell
{ N=1 }                                   { N=1 }
  [[1],[2],[3],[4],[5]]                   [[[1]],[[2]],[[3]],[[4]],[[5]]]
       ^^^^^^^^^^^^^^^ -> tailN1_1  =>           ^^^^^^^^^^^^^^^^^^^^^^^^
           ^^^^^^^^^^^ -> tailN1_2                     ^^^^^^^^^^^^^^^^^^
               ^^^^^^^ -> tailN1_3                            ^^^^^^^^^^^
{ N=1, Part = 2 } : N/A

----------------------------------------------------------------

{ N=2, P=1 }
  [ 1 : [t] | t <- concat tailN1_1 ] => [[1,2],[1,3],[1,4],[1,5]]
{ N=2, P=2 }
  [ 2 : [t] | t <- concat tailN1_2 ] => [[2,3],[2,4],[2,5]]
{ N=2, P=3 }
  [ 3 : [t] | t <- concat tailN1_3 ] => [[3,4],[3,5]]
{ N=2, P=4 }
  [ 4 : [t] | t <- concat tailN1_4 ] => [[4,5]]
{ N=2, P=5 }
  N/A as tailN1P1_5 == []
```

`combinations1'` is also required to be modified.
\begin{code}
combinations1' :: [a] -> [[[a]]]
combinations1' ms = [ [[m]] | m <- ms ]
\end{code}

and `combinations1` is equivalent to previous implementation but
, In another ~~words~~ codes, it means that flattened version of `combinations1'`.

```haskell
combinations1 :: [a] -> [[a]]
combinations1 = concat . combinations1'
```
== Tail After Tail !!

As you can see, as we are making combinations by selecting number of *N*,
we are actually preparing for next one. This is why I call the method
*Tail After Tail*.

== About Accumulation

And I found `scanl` is doing  something similar jobs.
if we want to use `scanr` for whatever reason,
we need to change our order of list a little bit. but for  <now. I'd like to
keep use `scanl`.

= How To Make Each Partital Result

So when you make *{N=2}* partial results, we need **total** result of *{N=1}*,
which depends on the `members`. which can be made from `combinations'`

So when we are making *{N=2}* cases, we will omit first one
(which is `[[1],...,[5]]`) and map over the `members`.
we are going to make function we can call for each part.

== genPart

`genPart` makes each partial result.

\begin{code}
genPart :: Foldable t => a -> t [[a]] -> [[a]]
genPart leader followerGroups = [ leader : followers
                                  | followers <- concat followerGroups ]
\end{code}

```haskell
λ> currentLeaderNum = 1
λ> followers = ((combinations' members) !! 1)
genPart currentLeaderNum followers
[[1,2],[1,3],[1,4],[1,5]]
λ> currentLeaderNum = 2
λ> followers = ((combinations' members) !! 2)
```

As we can see leader are increasing and followers *index* are also increasing.
which introduce us some function with parallel association.

We are going to use `zipWith` in this case.

== genStep

`genStep` : makes all partial results along with leader and followers.


\begin{code}
usefulTails :: [a] -> [[a]]
usefulTails = init . tails

genStep :: [[[a]]] -> [a] -> [[[a]]]
genStep prevTails members' =
  zipWith genPart members' (usefulTails prevTails')
  where
    prevTails' = tail prevTails -- head is not useful
\end{code}
So, `zipWith` will call `genPart` with each member of `members` and each member
of (tail prevTails) in parallel.

So with result of {N=1}, we could get {N=2}

```haskell
λ> combinationsN1 = combinations' members
λ> mapM_ print $ genStep combinationsN1 members
[[1,2],[1,3],[1,4],[1,5]]
[[2,3],[2,4],[2,5]]
[[3,4],[3,5]]
[[4,5]]
```
Please, note that the result is not flattened yet as we need to use tail of
the list later. Once flattened, we can hardly get the list we want.

Now it's time to accumlate them.

== scanl

Let's see the type first.

```haskell
λ> :t scanl
scanl :: (b -> a -> b) -> b -> [a] -> [b]
λ>
```

One thing obvious is that genStep will be the `(b -> a -> b)`.

and we have `seed` status from `(combinations' members)`.

But how about last arugment: `[a]`??

== membersTails

`membersTails` will make a list of leader group for each calling of `genStep`.

As step is growing up from number of one, the list will be reduced from the
tail. and `inits` will do the job and actually we need to reverse after all.

\begin{code}
membersTails = reverse . tail . inits -- tail is used to skip empty list.
\end{code}

== allCombinations''

allCombinations'' will produce **un-flattened** combinations for each step.
allCombinations'' will use `scanl` and let's check the value before going further.

=== foldable group for scanl

```haskell
λ> mapM_ print $ membersTails members
[1,2,3,4,5]
[1,2,3,4]
[1,2,3]
[1,2]
[1]
```

After wiring those up:

\begin{code}
allCombinations'' :: [a] -> [[[[a]]]]
allCombinations'' ms = scanl genStep (combinations1' ms) (membersTails ms)
\end{code}

*Looks quite simple, doesn't it?*

== allCombinations

allCombinations will produce flattened combinations which is we really want.
which look a bit tricky. but let's see our {N=2} result.

```haskell
[ [ [1,2],[1,3],[1,4],[1,5] ]   -- > group 1
, [ [2,3],[2,4],[2,5] ]         -- > group 2
, [ [3,4],[3,5] ]               -- > group 3
, [ [4,5] ]                     -- > group 4
]
```
*re-indented them for better reading*

In short:

```haskell
[ ...
  [ group1, group2, group3, group4 ] -- > N2
  ...
]
```

What we need first is

```haskell
group1 <> group2 <> group3 <> group4
```

And next one is

```haskell
N1 <> N2 <> N3 <> N4 <> N5
```

\begin{code}
allCombinations' = map concat . allCombinations''
allCombinations  = concat . allCombinations'
\end{code}

*Note:* `allCombinations'` will be used later.

= subsequences

Data.List module has a function called `subsequences`, which makes
all possilbe combinations from given list.

However, personally I am not big fan of the order which `subsequences`
creating.  What I prefer is the same as what `raku` language does.

== Raku's Combinations

Online doc: [combinations](https://docs.raku.org/routine/combinations)

```perl
 > [1..5].combinations
 (()
  (1) (2) (3) (4) (5)
  (1 2) (1 3) (1 4) (1 5) (2 3) (2 4) (2 5) (3 4) (3 5) (4 5)
  (1 2 3) (1 2 4) (1 2 5) (1 3 4) (1 3 5) (1 4 5) (2 3 4) (2 3 5) (2 4 5) (3 4 5)
  (1 2 3 4) (1 2 3 5) (1 2 4 5) (1 3 4 5) (2 3 4 5)
  (1 2 3 4 5)
 )
```

Preceding order makes more sense for me. Nevertheless, if you need quick
all combinations, you can try for `subsequences` because it comes with basic
haskell installation.

= Combination of K

Now, it's time to talk about select only K member(s) out of all.

i.e how to select **three** elements from a list of [ 'A', 'B', 'C', 'D', 'E' ]?

== laziness

```haskell
λ> take 0 [1..]
```

The preceding code works immediately because we don't need to know whole list
to get nothing from it. So we could get benefit from this laziness here again!

\begin{code}
combinations :: [a] -> Int -> Int -> [[a]]
combinations ms n1@selectFrom n2@selectTo =
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
    $ allCombinations' ms
\end{code}

Okay. This is it. I like this implementation because we recycle the previous
result to make new one. (What a sustainable choice! 🌎)

I hope performance is also nice. (less time -> less electricity?)

= About Compact Code _ 압축된 코드에 관해

For curiosity, I wrote down the full code of `allCombinations`
by using `let` or `where` clause.

```haskell
allCombinations =
  concat . map concat . allCombinations''
  where
    allCombinations'' ms =
      scanl genStep [ [[m]] | m <- ms ] (reverse . tail . inits $ ms)
      where
        genPart x tss = [ x : ts | ts <- concat tss ]
        genStep pt ms = zipWith genPart ms (init . tails . tail $ pt)
```

Yeah... haskell code naturally can be pretty compact.
But I believe that more documentation and best practice will haskell greater!

You can see this article in raw format at [here](https://github.com/jeongoon/jeongoon.github.io/blob/main/posts/2022-04-03-Combinations-TailAfterTail.lhs).

Thank you for reading!
