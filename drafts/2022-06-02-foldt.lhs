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
to make recursive call. Basically the *first element of leftmost group* has lowest value,
so it will be the first element in the result. This is the basic concept of `foldt`.

`unionSort` will remove duplicated member and take the element at lowest value
out of the both list.

=== unionSort

unionSort :: Ord a => [a] -> [a] -> [a]
unionSort [] ys = ys
unionSort xs [] = xs
unionSort xs@(x:xt) ys@(y:yt) =
  case x `compare` y of
    LT -> x : unionSort xt ys -- x used; do unionSort on rest of them
    EQ -> x : unionSort xt yt -- x used; y is duplicated
    GT -> y : unionSort xs yt

\end{code}

`unionSort` also has recursive call to finish the `union` and `sort` on rest of
members which depends on the value is choosen for the first place.

=== pairs

`pairs` do the same sort method which is used in `foldt`. `foldt` takes only
one group each time, but `pairs` on the other hand tries to take two groups
each time. If it cannot, it returns empty or the leftmost group.

\begin{code}

pairs :: Ord a => [[a]] -> [[a]]
-- edge cases ...
pairs [] = []
pairs ([]:_) = [] -- left always has longer list; no need to go further
pairs (ms:[]) = [ms] -- just return leftmost group

\end{code}

pairs ((m:ms):ns:t') = (m : unionSort ms ns) : pairs t'
\end{code}

Or if it can take two groups, `m` will be the lowest value and do `unionSort`
on rest of between two groups, and `pairs` will bite the tail of the code again
to finish the job.

== foldt examples

=== sumOfMultiples

This task is introduced at [exercism.org](https://exercism.org/tracks/haskell/exercises/sum-of-multiples).
Even though, we could solve this problem by checking divisibility of all the
member numbers in given `factor` list, it was worth to try because `foldt` is
fast enough to solve by *union*ing the numbers and get only one of common
multiples.

\begin{code}
sumOfMultiples factors limit =
  sum $ foldt [ [n,n+n..(limit -1)] | n <- factors', n > 0 ]
  where factors' = sort factors
\end{code}

```haskell
λ= sumOfMultiples [2,3,5,7,11] 100000
3961010983
```

=== original prime method

I'll just leave the whole code for now. Too many recursive call
probably makes us confused at first. but if you know how foldt works
in there, it will be easier to figure out how it works!

To be honest, This code is still hard for me to understand. Or I could
understand but I don't think I could invent something like this. 😅

The basic idea of the generating is that if I found a prime,
`prime square` and `prime * (prime + 1)` couldn't be prime numbers.
so will be removed from the prime list.

```haskell
module PrimeNumber
  ( primesTME
  ) where

primesTME :: [Int]
primesTME =   2 : ([3,5..] `minus` foldt [ [p*p,p*p+2*p..] | p <- primes_ ])
  where
    primes_ = 3 : ([5,7..] `minus` foldt [ [p*p,p*p+2*p..] | p <- primes_ ])
    foldt ~((x:xs):t) = x : union xs (foldt (pairs t))
    pairs ~((x:xs):ys:t) = (x : union xs ys) : pairs t

minus :: [Int] -> [Int] -> [Int]
minus xs@(x:xt) ys@(y:yt) = case compare x y of
                              LT -> x : minus xt ys
                              EQ ->     minus xt yt
                              GT ->     minus xs yt

union :: [Int] -> [Int] -> [Int]
union xs@(x:xt) ys@(y:yt) = case compare x y of
                              LT -> x : union xt ys
                              EQ -> x : union xt yt
                              GT -> y : union xs yt

```

As you can see `foldt` and `pairs` are now much simpler because it works
on the infinite list.
