---
title: Combinations in haskell
description: solving 99 question on haskell.org and #083 Task2 in perlweeklychallenge.org
keywords: haskell, combinations, raku, documentation
author: Myoungjin Jeon
tags: haskell, combinations, raku, documentation
---
Copyright (c) 2020 JEON Myoungjin <jeongoon@g... >

Another Combination Module from me
==================================

This module was creaeted when I solve [The Perl Weekly Challange #083](https://perlweeklychallenge.org/blog/perl-weekly-challenge-083/#TASK2).

I made another implementation for imperative method long time ago, which results
in satisfying enough in pure perl code. However, It was impossible to achieve
the same result in haskell because it is not natural for haskell to approach
the similar (imperataive) way.
But *I'm not saying this is the best combination solution*, I'd rather it looks
a bit tricky and ugly. In comparison in solution in
[99 Questions](https://wiki.haskell.org/99_questions/Solutions/26),
this module shows relatively faster result.


Documentation Is Always Good Idea
=================================

However, I didn't write down any comments on it. Now the person who is
suffurring to understand what the code exacatly does is myself. 😂
And no commentary or documentation always makes Haskell
striving for getting popularity, IMHO.

So I changed the format into literate haskell and try to blog about it.
at the same time, I could get another chance to writing a blog in literate
haskell to find some missing-feature and workaround.


This is a simple module and not a academic module either.
However I found something is useful or similar to another language.

First of all, we need to declare what is the module name and
write down the which function or data type we will export.

\begin{code}
module Combinations
  ( combinations
  ) where
\end{code}

This module makes serveral implementation with `pattern matching`
under the same function name. But overall code should satisfy the
type annotation and cover all the cases it could happen.

So type annotation will give us the hint of look and feel of function. 🤔

**Note:** argument order is opposite from the [99 questions on haskell.org.](https://wiki.haskell.org/99_questions/Solutions/26)
which is actually good order for [partial application](http://learnyouahaskell.com/higher-order-functions#curried-functions).
However, other language -- let's say python -- normally has the order of
what I put, which was easier for me during solving the PWC #083 Task#2.

\begin{code}
combinations :: [a] -> Int -> [[a]]
\end{code}

First thing we could imagine is that if there is nothing to select.

\begin{code}
combinations []     _  = []
\end{code}

Yes, empty list is the only the answer.

How about if we have at least one candidate and choose one from them?

There are so many ways to do it. but below code will serve enough.

\begin{code}
combinations (m:ms) 1 = [m] : (combinations ms 1)
\end{code}

last (combinations ms 1) will results in [[a]], so entire result
also has tye same type as [[a]].

*BTW, Another variation might be like below.*

```haskell
combinations ms 1 = [ [m] | m <- ms ]
```

*which I think more human-friendly syntax.*

I add some pattern matching for better performance.
which is `select 2 out of the choices`.

\begin{code}
combinations [_]    2 = []
combinations [e,f]  2 = [[e,f]]
\end{code}

And `sequence` function shows good performance on this as well.
I used it.
\begin{code}
combinations (m:ms) 2 = sequence [[m], ms] ++ (combinations ms 2)
\end{code}

Pattern matching in Raku
========================

Pattern matching in [Raku](www.raku.org) also has similar pattern.
I guess this idea coming from haskell or C++

```perl
sub combinations([], Any) { [] }
sub combinations( Array $ms is copy, 1 ) {
    my $m = $ms.shift;
    [[$m]]
    .append( [samewith( $ms, 1 ) ]
    );
}
```

BTW, raku has its own combination function for user. handy!

Lastly, I wrote for the other general cases.
\begin{code}
combinations mList  n =
  case totalLen `compare` n of
\end{code}

And I could gain an insight of the pattern from above choice of 1 or 2.
If we are trying to choose from empty list, result is empty list.
And if the number of choice and the number of candidates are the same,
return a list of one element which selects everything in the candidates.

\begin{code}
    LT -> []
    EQ -> [mList]
\end{code}

So now we did cover the number of choice up to `2`.
And I build up the pattern from num. of choice `3`.

```ascii

-- for easier writing flip the order of arguments.
combinations' = flip combinations
mList = [1,2,3,4,5]

      mList                         mList
        |                             |
  combinations' 1              combinations' 3
        |                             |
        v                             v
[ [1],                     [ [1] ++ comb |
                               comb <- combinations' 2 [2,3,4,5] ]
                           ++
  [2],                     [ [2] ++ comb |
                               comb <- combinations' 2 [3,4,5] ]
                           ++
  [3],
  [4],                     ...
  [5],
]
```

And finally generalized form of the function is below.

\begin{code}
    _  -> [ let leaders = map fst tups
            in  leaders ++ followers |
            tups <- combinations (zip mList [0 .. room])  n',
            let skipCount = ((snd.last) tups) + 1,
                followers <- (combinations (drop skipCount mList) 2) ]
  where
    totalLen    = length mList
    room        = totalLen - 2
    n'          = n - 2
\end{code}

If you look into full code without comments:

```haskell
module Combinations
  ( combinations
  ) where

combinations :: [a] -> Int -> [[a]]
combinations []     _  = []
combinations (m:ms) 1 = [m] : (combinations ms 1)
combinations [_]    2 = []
combinations [e,f]  2 = [[e,f]]
combinations (m:ms) 2 = sequence [[m], ms] ++ (combinations ms 2)
combinations mList  n =
  case totalLen `compare` n of
    LT -> []
    EQ -> [mList]
    _  -> [ let leaders = map fst tups
            in  leaders ++ followers |
            tups <- combinations (zip mList [0 .. room])  n',
            let skipCount = ((snd.last) tups) + 1,
                followers <- (combinations (drop skipCount mList) 2) ]
  where
    totalLen    = length mList
    room        = totalLen - 2
    n'          = n - 2

```

It's not very clear what I was trying to say in this code.
Because I'm a novice haskeller and so does other haskellers.
Why don't you use literate format if you can?


And About Usage
===============

It would be more than happy if I find how I could actually use them
on documentation, wouldn't it?

Probably `ghci` is the best for quick testing!

```sh
sh> ghci
λ> :l Combinations.hs
[1 of 1] Compiling Combinations     ( Combinations.hs, interpreted )
Ok, one module loaded.
λ> combinations [1,2,3,4,5] 3
[[1,2,3],[1,2,4],[1,2,5],[1,3,4],[1,3,5],[1,4,5],[2,3,4],[2,3,5],[2,4,5],[3,4,5]]
```

Documentation is important. and "literate haskell" helps me a lot to do it.

Okay. that's all for today! 