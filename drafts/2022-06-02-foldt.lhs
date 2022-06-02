---
title: Union Fold Tree
description: explain how the fold the series of series of numbers in lazy way
keywords: fold, union, haskell, foldt
author: Myoungjin Jeon
---

== Where folding-tree was used

I found this method in genrating prime numbers. which I hardly understand at the
time, even though it looks very interesting. fortuneately I had better
understanding about it so I'd like to share.

\begin{code}
module UnionFoldTree (foldt) where

import Data.List (sort)

foldt :: Ord a => [[a]] -> [a]
\end{code}

=== foldt

`foldt` takes a list of list of series of numbers and return a folded
(union) and sorted list. however has some assumption shown below:

- Each list, which is inside the list, contains sorted already in ascending order
- All the groups is sorted by order of The first element of each group.

So If we have multiples of number three and five and seven,

```haskell
λ> make_some_multiples n = [ n, n+n .. 20 ]
λ> three_multiples = make_some_multiples 3
λ> take 5 $ three_multiples
[3,6,9,12,15,18]
λ> -- ^ already sorted
λ> five_multiples = make_some_multiples 5
λ> seven_multiples = make_some_multiples 7
```

We need to apply the list of each multiples in this way:

```haskell
λ> foldt [ three_multiples, five_multiples, seven_multiples ]
```

The original foldt is for the infinite list has less condition,
however, I'd like to apply foldt to fixed size of list so has more
edge cases:

\begin{code}
foldt [] = []
foldt ([]:_) = []
\end{code}

So, on second condition, we can reutrn empty list if the list of left hand side
has empty body. because each multiples has shorter length of list if the initial
number is larger.

\begin{code}
foldt ((x:xs):t) = x : unionSort xs (foldt (pairs t))
\end{code}

The last is for general condition and as you can see foldt appears in the end again
to make recursive call. Basically the first element of leftmost group has lowest value,
so it will be the first element in the result. `unionSort` will remove duplicated
member and take the element at lowest value out of the both list.

=== unionSort

unionSort :: Ord a => [a] -> [a] -> [a]
unionSort [] ys = ys
unionSort xs [] = xs
unionSort xs@(x:xt) ys@(y:yt) =
  case x `compare` y of
    LT -> x : unionSort xt ys
    EQ -> x : unionSort xt yt
    GT -> y : unionSort xs yt

\end{code}

`unionSort` also has recursive call to finish the `union` and `sort` on rest of
members which depends on the value is choosen for the first place.

\begin{code}


pairs :: Ord a => [[a]] -> [[a]]
-- edge cases ...
pairs [] = []
pairs ([]:_) = [] -- left always has longer list; no need to go further
pairs (ms:[]) = [ms]

pairs ((m:ms):ns:t') = (m : unionSort ms ns) : pairs t'



sumOfMultiples :: [Integer] -> Integer -> Integer

sumOfMultiples factors limit =
  sum $ foldt [ [n,n+n..(limit -1)] | n <- factors', n > 0 ]
  where factors' = sort factors

\end{code}
