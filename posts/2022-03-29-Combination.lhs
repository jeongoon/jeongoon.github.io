---
title: Combinations in haskell (called `Leaders and Followers`)
description: solving 99 question on haskell.org and #083 Task2 in perlweeklychallenge.org
keywords: haskell, combinations, raku, documentation
author: Myoungjin Jeon
---
Copyright (c) 2020 JEON Myoungjin <jeongoon@g... >
LICENSE: Open Software License 3.0

== Combinations in Haskell Series

   1. *Combinations In Haskell (called Leaders and Followers)*
   2. [Combinations In Haskell (called Tail After Tail)](/posts/2022-04-03-Combinations-TailAfterTail.html)
   3. [Tail After Tail Story (Combinations In Haskell)](/posts/2022-04-15-Combinations-TailAfterTail'.html)

= Another Combination Module from me

This module was created when I solve [The Perl Weekly Challange #083](https://perlweeklychallenge.org/blog/perl-weekly-challenge-083/#TASK2).

I made another implementation for imperative method long time ago, which results
in satisfying speed for general ussage in pure perl code. However it was
impossible to achieve the same result in haskell due to nature of the language.
(recursive is the basic step for haskell)

But *I'm not saying this is the best combination solution*, I'd rather it looks
a bit tricky and ugly. In comparison in solution in
[99 Questions](https://wiki.haskell.org/99_questions/Solutions/26),
this module shows relatively faster result.


= Documentation Is Always Good Idea


However, I didn't write down any comments on it. Now the person who is
sufferring to understand what the code exacatly does is myself. 😂
And no commentary or documentation always makes Haskell
striving for getting popularity, IMHO.
<br><br>

So I changed the format into literate haskell and try to blog about it.
At the same time, I could get another chance to write a blog in literate
haskell to find some missing-feature and workaround.

This is a simple module and not a academic module either.
However I found something is useful or similar to another language.

= Module Starts

First of all, we need to declare what is the module name and
write down the which function or data type we will export.
My humble module has only one function named `combinations`.

\begin{code}
module Combinations
  ( combinations
  ) where
\end{code}

This module makes serveral implementation with `pattern matching`
under the same function name. But overall code should satisfy the
type annotation and cover all the cases it could happen.

So type annotation will give us the hint of look and feel of function. 🤔

**Note:** the order of argument is opposite from the [99 questions on haskell.org.](https://wiki.haskell.org/99_questions/Solutions/26)
which is actually better order for [partial application](http://learnyouahaskell.com/higher-order-functions#curried-functions).
However, other language -- let's say python -- normally has the order of
as I put, which was easier for me to write the same order during solving the PWC #083 Task#2.

\begin{code}
combinations :: [a] -> Int -> [[a]]
\end{code}

First thing we could imagine is that if there is nothing to select.

Empty candidates -> Empty results
---------------------------------

\begin{code}
combinations []     _  = []
\end{code}

Yes, empty list is the only the answer.

How about if we have at least one candidate and choose one from them?

There are so many ways to do it. but below code will serve enough.

== Only One Choice

\begin{code}
combinations ms 1 = [ [m] | m <- ms ]
\end{code}

last (combinations ms 1) will results in [[a]], so entire result
also has tye same type as [[a]].

*BTW, Another variation might be like below.*

```haskell
combinations (m:ms) 1 = [m] : (combinations ms 1)
```

Which was actually my previous code. but I found that this code evaluated
lazily and too many unnecessary function call required.
(I could get 17% performace increased after replace the code)

== Select Two Out of Many

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

== Pattern matching in Another Language - Raku


This is what I found the similarity in pattern matching.
Pattern matching in [Raku](www.raku.org) also has similar structure.
~~I guess this idea came from haskell or C++~~

```perl
 # combinations []     _  = []
 multi combinations( [] , Any ) { [] }

 # combinations (m:ms) 1 = [m] : (combinations ms 1)
 multi combinations( Array $ms is copy, 1 ) {
     my $m = $ms.shift;
     [ [$m] ]
         .append( [samewith( $ms, 1 ) ]
     );
 }
```

Haskell code looks tidy and clean here. because it is designed to use
pattern matching a lot!
BTW, raku has its own [combinations](https://docs.raku.org/routine/combinations)
function for user. handy!
<br><br>

== Next! Next! and General


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

And finally extend the idea I had above and made a generalized form of the function.

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
combinations ms     1  = [ [m] | m <- ms ]
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

Maybe you could guess what is going on now. but if you started read
the no-comment-code from the beginning, It might be not very clear
what I was trying to say.

Because I'm a novice haskeller and so does other haskellers.
Why don't you use literate format if you can?


= About Usage  사용법에 대해

It would be more than happy if I find how I could actually use them
on documentation, wouldn't it?

Probably `ghci` is the best for quick testing!

save [this my blog code](https://raw.githubusercontent.com/jeongoon/jeongoon.github.io/main/posts/2022-03-29-Combination.lhs) as `Combinations.lhs` and ...

```sh
sh> ghci
λ> :l Combinations.lhs
[1 of 1] Compiling Combinations     ( Combinations.lhs, interpreted )
Ok, one module loaded.
λ> combinations [1,2,3,4,5] 3
[[1,2,3],[1,2,4],[1,2,5],[1,3,4],[1,3,5],[1,4,5],[2,3,4],[2,3,5],[2,4,5],[3,4,5]]
```

Documentation is important. and "literate haskell" helps me a lot to do it.

If you want to look at the raw file format: [see this](https://github.com/jeongoon/jeongoon.github.io/blob/main/posts/2022-03-29-Combination.lhs)

You could get basic idea how to write down literate haskell and blog about it.

Okay. that's all for today!
