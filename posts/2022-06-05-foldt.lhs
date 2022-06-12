---
title: Let's Fold Tree
description: explain how the fold the series of series of numbers in lazy way
keywords: fold, union, haskell, foldt
author: Myoungjin Jeon
---

= folding-tree?

 >  *The term of `folding-tree` is not an official or common name. But I'll
 >  call the function as `folding-tree` here.* I guess `Tree Merging` is more
 >  accurate term. [see more](https://wiki.haskell.org/Prime_numbers#Tree_merging).
 >
 >  ~~I lost the original source code because link I have is broken now. So this article is licensed under MIT as I borrow from other's~~

= Credit Update

I found the original [link](https://wiki.haskell.org/Prime_numbers)!
and What I saw for the first time is [this](https://ideone.com/p0e81).

I found this function in another function generating prime numbers.

which I hardly understand at the time, even though it looks very interesting.

== How to use (test)
Fortunately I have better understanding about it so I'd like to share.
This source code is written literate haskell and you try to use in ~ghci~
like below:

```sh
shell> curl -sL https://raw.githubusercontent.com/jeongoon/jeongoon.github.io/main/posts/2022-06-05-foldt.lhs -o FoldTree.lhs
shell> ghci
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/myoungjin/.config/ghc/ghci.conf
λ= :l FoldTree.lhs 
[1 of 1] Compiling FoldTree         ( FoldTree.lhs, interpreted )
Ok, one module loaded.
λ= take 7 . foldt $ [[3,6..], [5,10..], [7,14..]]
[3,5,6,7,9,10,12]
```

== Module FoldTree

\begin{code}
module FoldTree (foldt) where

import Data.List (sort)

foldt :: Ord a => [[a]] -> [a]
\end{code}

=== foldt

`foldt` takes a list of list of series of numbers and return a merged
(union) and sorted list. It is more like making a rope with the strings.

However, there are some assumption about usage:

- Each inner list contains sorted already in ascending order
- All the groups is sorted by order of the first element of each group.

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

 > Note: In general, if your implementation has specific limitation on input
 > value, or you might need to write the funciton name as something imposing
 > the limitation or your own data type.
 >
 > Another option would be phanthom type[^1].

We need to apply the list of each multiples in this way:

```haskell
λ> foldt [ three_multiples, five_multiples, seven_multiples ]
```

This assumption helps for our *thunk[^2]s* to care about only the the list in
the future, which goes pretty well with the nature of lazy evaluation!

The original `foldt` impentation is for the infinite list has less condition,
however, I'd like to apply foldt to fixed size of list so has more
edge cases:

\begin{code}
foldt [] = []
foldt ([]:_) = []
\end{code}

So, on second condition, we can return empty list if the list of left hand side
has empty body. because each multiples has shorter length of list if the initial
number is larger.

\begin{code}
foldt ((x:xs):t) = x : unionSort xs (foldt (pairs t))
\end{code}

The last pattern is for general condition. And foldt appears in the end again to
make recursive call.

Basically the *first element of leftmost group* has lowest value,
so it will be the first element in the result. This is the basic concept of `foldt`.
And rest of list (xs) will be union-ed with rest of `foldt`-ed list.

`unionSort` will remove duplicated member and take the element at lowest value
out of the both list.

=== unionSort

\begin{code}
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
members which depend on the value choosen for the ~head~ of result.

=== pairs

`pairs` do the same sort method used in `foldt`. `foldt` takes only
one group each time, on the other hand, `pairs` tries to take every two groups
each time. If the pair is not available, it returns empty or the *leftmost* group
so that `foldt` will end its job earlier.

\begin{code}
pairs :: Ord a => [[a]] -> [[a]]
-- edge cases ...
pairs [] = []
pairs ([]:_) = [] -- left always has longer list; no need to go further
pairs (ms:[]) = [ms] -- just return leftmost group
\end{code}

The second pattern matching will reduce the searching time as well -- as
we saw in the pattern matching of `foldt`.

*Those edge cases could be different when the different types of sereies of numbers
are used.*

\begin{code}
pairs ((m:ms):ns:t') = (m : unionSort ms ns) : pairs t'
\end{code}

If two groups are taken to perform union, `m` is supposed to be the lowest value
and will be the first value of `pairs` function -- which is actually doing
amazing job to wiring all the `foldt` and `pairs` smoothly because it doesn't
need to go further to get first value.

 > Note: there is no doubt that less evaluation tends to be more efficient in
 > lazy computation.

To organize the rest of them `unionSort` will be applied on rest of between
wo groups, and `pairs` will persue the tail of the code again to finish the job.

This is one of most beautiful piece of recursive programming.

== foldt examples

=== sumOfMultiples

This task is introduced at [exercism.org](https://exercism.org/tracks/haskell/exercises/sum-of-multiples).
Even though, we could solve this problem by checking divisibility of all the
member numbers which is given as `factor` list, it was worth to try because `foldt` is
fast enough to solve by *union*-ing the numbers and get only one of common
multiples.

\begin{code}
sumOfMultiples factors limit =
  sum $ foldt [ [n,n+n..(limit -1)] | n <- factors', n > 0 ]
  where factors' = sort factors
  -- note: sort is reqiuired due to the foldt has assumption!
\end{code}

```haskell
λ= sumOfMultiples [2,3,5,7,11] 100000
3961010983
```

=== origin of foldt - prime numbers

 > original file contains a lot more implementation of prime numbers[^3].

Too many recursive call probably makes us confused at first. But
if you know how foldt acts in there, it will be easier to figure out how it works!

To be honest, This code is still hard for me to understand. Or even if I could
understand it, I don't think I could invent something like this. 😅

The basic idea of the generating is called [`Sieve of Eratosthenes`](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes). But foldt acomplish the task in unique way
to perform sieving when compared to imperative [pseudo code](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes#Pseudocode).

We could think `minus` function is to used sieve and `foldt` function used to eliminate
the duplication and to sort.

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
on the infinite list. and `~` prefix will calm down the `ghc` because it tells
`ghc` to trust that our pattern matching will serve all the cases even though
it doesn't look like exhaustive.

 > I was able to compile this module without `~`. I guess that there are some
 > still possibility for older ghc complier to complain about non-exhaustive
 > pattern matching.

== What we can do more?

I was thinking about making business hours by using `foldt`. A set of trading hours
could rely the public holiday and day light saving time if applicable. (because
most of times, UTC will be prefered to save time information)
or each weekday could have the different schedule like *"shopping day"*
(which is Thursday in Australia).

So we could have many groups of time tables and get the final trading hours
list by applying union and sort them and also fitler them by removing some
special occasion or public holidays!

It sounds the usage of `foldt` is suitable for this tasks, doesn't it?

I hope I could write an article about it! Thank you for reading!

== Foot Notes

[^1]: [Phantom type from wiki.haskell.org](https://wiki.haskell.org/Phantom_type)
[^2]: [Thunk from wikiepedia](https://en.wikipedia.org/wiki/Thunk)
[^3]: [original source](https://ideone.com/p0e81)
