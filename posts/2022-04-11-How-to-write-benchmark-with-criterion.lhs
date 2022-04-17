---
title: Benchmark With Criterion
description: writing benchmark code in haskell with criterion
keywords: haskell, benchmark, hpack, criterion
author: Myoungjin Jeon
---

== Benchamark for Beginner Series
   1. [Haskell] [Stack Setup with pacakge.yaml](/posts/2022-04-10-How-to-write-benchmark-with-stack.html)
   2. [Haskell] *Write Benchmark with **Criterion***
   3. [Go] [Builtin Benchmark with Go](/posts/2022-04-13-How-to-write-benchmark-in-golang.html)

== credits

   - I found the criterion information at [https://mmhaskell.com/testing/profiling]
     however example doesn't show how to write down *package.yaml*

I wrote about [how to create benchmark programme with stack](https://jeongoon.github.io/posts/2022-04-10-How-to-write-benchmark-with-stack.html) package tool.

And file is an example of benchmark code in haskell with package [criterion](https://github.com/haskell/criterion).

And this module could be a *App*, we need to declare as **Main**

\begin{code}
module Main where

import Criterion
import Criterion.Main (defaultMain)
\end{code}

== Load Modules To Test

To test how fast my combinations module are, I made serveral individual module
per combinations method.

you could access codes at [here](https://github.com/jeongoon/combinations-bench/tree/main/haskell-combinations/src)

\begin{code}
import qualified TailAfterTail as Tat
import qualified LeadersAndFollowers as Laf
import qualified DynamicProgramming as Dyn
\end{code}

== Prepare Sample Members

Some Numbers are used to be tested with `criterion`.

\begin{code}
small, medium, large :: [Int]
small  = [1..5]
medium = [1..12]
large  = [1..22]
\end{code}

== Helper Function and Data Types

In Elm language, not to confuse the order of argument,
it forces us  to write a data type, especially when the function
has more than three arguments. And I believe that in haskell, it will be
the good practice as well.

\begin{code}
data TestData a =
  TestData
  { name :: String
  , combiFunction :: [a] -> [[a]]
  , sample :: [a]
  }
\end{code}

=== benchAllCombo

`benchAllCombo` will make `Benchmark` type will be applied to `bgroup` function.

\begin{code}
benchAllCombo (TestData name combiFunction sample) =
  bench name $ nf combiFunction sample
\end{code}

=== mkAllCombinationsWith

`mkAllCombinationsWith` is a wrapper function to create all possible
combinations out of the members when the combination module doesn't provide one.

\begin{code}
mkAllCombinationsWith combiFunction ms =
  concat [ combiFunction ms n | n <- [1..length ms] ]
\end{code}

== main

`main` function is the executable code block for this benchmark.

defaultMain from `Criterion` will do rest of the job for us to handle
any arguments and execute each benchmark group (`bgroup`)


\begin{code}
main :: IO ()
main = do
  defaultMain
\end{code}

=== bench

[bench](https://hackage.haskell.org/package/criterion-1.5.9.0/docs/Criterion.html#v:bench)
will create a `Benchmark` type but *please note that*

You *should* pass:

1. A function to test
2. An agrument to be applied.

In other words, you should not do:

```haskell
bench "test benchmark message" $ nf const testFunctionWithoutArgument
```

Rather you should do:

```haskell
bench "test benchmark message" $ nf testFunction anArgument
```

*`const` is a useful function as much as `id` to mould a function
to make it suitable in a different *context* like in prior example.
(a function need one argument -> a function without any)

*nf* stands for `normal form` and another one is `weak head normal form`.
I skipped this explanation as I don't have enough knowledge to share yet.

\begin{code}
    [ bgroup "Small Sample Comparison"
      [ benchAllCombo TestData { name = "Leaders and Followers"
                               , combiFunction = mkAllCombinationsWith Laf.combinations
                               , sample = small }
      , benchAllCombo TestData { name = "Tail After Tail"
                               , combiFunction = Tat.allCombinations
                               , sample = small }
      , benchAllCombo TestData { name = "Dynamic Programming"
                               , combiFunction = Dyn.allCombinations
                               , sample = small }
      ]

      , bgroup "Medium Sample Comparison"
      [ benchAllCombo TestData { name = "Leaders and Followers"
                               , combiFunction = mkAllCombinationsWith Laf.combinations
                               , sample = medium }
      , benchAllCombo TestData { name = "Dynamic Programming 1"
                               , combiFunction = Dyn.allCombinations
                               , sample = medium }
      , benchAllCombo TestData { name = "Dynamic Programming 2"
                               , combiFunction = Dyn.allCombinations
                               , sample = medium }
      , benchAllCombo TestData { name = "Tail After Tail 1"
                               , combiFunction = Tat.allCombinations
                               , sample = medium }
      , benchAllCombo TestData { name = "Tail After Tail 2"
                               , combiFunction = Tat.allCombinations
                               , sample = medium }
      ]
      , bgroup "Large Sample Comparison"
      [ benchAllCombo TestData { name = "Tail After Tail 1"
                               , combiFunction = Tat.allCombinations
                               , sample = large }
      , benchAllCombo TestData { name = "Tail After Tail 2"
                               , combiFunction = Tat.allCombinations
                               , sample = large }
      , benchAllCombo TestData { name = "Dynamic Programming 1"
                               , combiFunction = Dyn.allCombinations
                               , sample = large }
      , benchAllCombo TestData { name = "Dynamic Programming 2"
                               , combiFunction = Dyn.allCombinations
                               , sample = large }
      , benchAllCombo TestData { name = "Leaders and Followers"
                               , combiFunction = mkAllCombinationsWith Laf.combinations
                               , sample = large }
      ]
    ]
\end{code}

It was a bit long and could have been written in helper function but I kept
changing the layout of benchmark so I didn't make one. and copied and paste. 😓

and you could run the test with `stack` in two ways:

=== bench target

```sh
sh> stack build haskell-combinations:bench:haskell-combinations-benchmark
sh> #                                ^^^^^ -> benchmark target
sh> ... will compile the code print out the benchmarks result

.. snip ..
Benchmark haskell-combinations-benchmark: RUNNING...
benchmarking Small Sample Comparison/Leaders and Followers
time                 4.006 μs   (3.987 μs .. 4.028 μs)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 4.011 μs   (3.996 μs .. 4.039 μs)
std dev              68.39 ns   (42.39 ns .. 96.35 ns)
variance introduced by outliers: 16% (moderately inflated)
                                  
benchmarking Small Sample Comparison/Tail After Tail
time                 2.556 μs   (2.527 μs .. 2.608 μs)
                     0.997 R²   (0.993 R² .. 1.000 R²)
mean                 2.560 μs   (2.538 μs .. 2.619 μs)
std dev              105.5 ns   (57.84 ns .. 189.1 ns)
variance introduced by outliers: 55% (severely inflated)
                                  
benchmarking Small Sample Comparison/Dynamic Programming
time                 3.061 μs   (3.049 μs .. 3.078 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 3.115 μs   (3.089 μs .. 3.194 μs)
std dev              130.2 ns   (35.74 ns .. 258.9 ns)
variance introduced by outliers: 55% (severely inflated)
```

**Note:** As you can see *criterion* try many times to ensure the result is reliable.
even though it is not perfect only because your system is not always in the
same condition.

=== executable target

```sh
 # compile first
 sh> stack build haskell-combinations:exe:haskell-combinations-benchmark-exe
 # execute
 sh> stack exec haskell-combinations-benchmark-exe -- -o output.html
 # and have a look `output.html`
```

`output.html` will report the result with some helpful graphs.

Okay. This is it.

I'll update if I found if there are more effective way to explain usage.
and about WHNF (Weak Head Normal Form).

This file is actually working source code written in *literate haskell*

You can access [here](https://github.com/jeongoon/jeongoon.github.io/blob/main/posts/2022-04-11-How-to-write-benchmark-with-criterion.lhs)
