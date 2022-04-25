---
title: Learning A Lanugage with Exercism
description: exercism.org provides a community for learning programming languages
keywords: exercism, haskell
author: Myoungjin Jeon
---

== Tasting Language vs Learning Lanugage

Tasting a language is easy one. Just look at the example source code
and run on my computer or online to play with it.

However serious studying is different story. I need to get deeper insight
during learning the languages, trying to apply them for real situation.

I'm still thinking which language I'll learn along with Haskell.
I have limited time and this is quite tough choice. My choice is leaning to
`rust` but at the same time want to learn C# or Dart. Something I can make
a application, web application and mobile apps if possible.

Only I worry about `rust` language, it is too complicated for me now to
understand until I could use it professionally, which is actually the same
problem I have with `Haskell`.

Anyway, I used to solve the [The Weekly Challange](https://theweeklychallenge.org/).
But I feel like I want to try something else at this time.

== Exercism for Learning Language

I saw someone making a git repository named `exercism` and today I finally
check what it is. It is community for learning by solving exercises posted
on the web site. One thing I really appriciate about is that I could test
via their web site as well.

== Leap Year

Given a year, report if it is a leap year.

The tricky thing here is that a leap year in the Gregorian calendar occurs:

on every year that is evenly divisible by 4
  except every year that is evenly divisible by 100
    unless the year is also evenly divisible by 400

For example, 1997 is not a leap year, but 1996 is. 1900 is not a leap year, but 2000 is.

== My Second iteration

I believe that this implementation is applicable in most of languages.

```haskell
module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = divisibleBy 4
                  && ( divisibleBy 400
                       || (not . divisibleBy $ 100) )
  where
    divisibleBy fac = year `rem` fac == 0
```

And then I look around others solution, and realized that it is more interesting
than I thought at first time.

== My fifth iteration

My 5th try is inspired by user *Pi*. she made custom operator called `unless`
and write down the solution in more english-like language.

But I'm not very familiar with her implementation of `unless` though.
So I made a variant with two different operators:
throu

\begin{code}
module LeapYear (isLeapYear) where

isLeapYear'' :: Integer -> Bool
isLeapYear'' year = (divisibleBy 4)
                   `except'` ( (divisibleBy 100)
                              `unless'` (divisibleBy 400) )

  where
    divisibleBy fac = year `rem` fac == 0

infixr `except'`
except' :: Bool -> Bool -> Bool
a `except'` b
  | a == False = False
  | otherwise  = not b

infixr `unless'`
unless' :: Bool -> Bool -> Bool
a `unless'` b
  | b == True = False
  | otherwise = a
\end{code}

== confusing `unless` concept

Then I realized something is not right even though it will give correct answer.

The original her implementation of unless looks like:

```haskell
infixr `unless`
unless :: (t -> Bool) -> (t -> Bool) -> t -> Bool
(p `unless` q) x = p x && not (q x)
```

However, `unless` in Perl5 is something like below:

```perl
unless ( someBool ) {
    // do or something if not someBool
}
```

This is not normal balanced if-then-else condition, rather it's unary condition.
we cannot what will come `someBool` is true because it is not defined.

So, my understanding is that return of unless is kind of Maybe type.

== My fifth' iteration

\begin{code}
infixr `unless`
unless :: Maybe a -> Maybe Bool -> Maybe a
p `unless` q
  | q == Just False = p
  | otherwise       = Nothing
\end{code}

So I could make `except` again

\begin{code}
infixr `except`
except :: Maybe a -> Maybe Bool -> Maybe a
p `except` q
  | q == Just True = Nothing
  | otherwise      = p
\end{code}

And Now I need to change my isLeapYear Again:

\begin{code}
isLeapYear' :: Integer -> Bool
isLeapYear' year =
  case mbLeapYear year of
    Just result -> result
    Nothing     -> False

  where
    mbLeapYear year =
      (mbDivisibleBy 4)
      `except` ( (mbDivisibleBy 100)
                 `unless` (mbDivisibleBy 400) )

    mbDivisibleBy fac
      | year `rem` fac == 0 = Just True
      | otherwise           = Just False
\end{code}

However if the year is divisible by 400, still we need to divide by 4 again.
Because:

```haskell
-- (mbDivisibleBy 4) `except` ( (mbDivisibleBy 100) `unless` (Maybe True) )
-- becomes:
(mbDivisibleBy 4) `except` Nothing
```

So I think I just go back to my 2nd iteration or declare another data type
to make all the sense through the codes.

\begin{code}
data YesOrNoAnswer = Yes
                   | No
                   | Unsure deriving (Eq)

infixr `ifUnsure`
ifUnsure :: YesOrNoAnswer -> YesOrNoAnswer -> YesOrNoAnswer
p `ifUnsure` q
  | q == Unsure = p
  | otherwise   = q

withDefault :: YesOrNoAnswer -> Bool -> Bool
ans `withDefault` def = case ans of
                          Yes    -> True
                          No     -> False
                          Unsure -> def

isLeapYear :: Integral a => a -> Bool
isLeapYear year =
  (checkDivisibleBy100
    `ifUnsure` checkDivisibleBy400) `withDefault` (divisibleBy 4)

  where
    divisibleBy fac = year `rem` fac == 0

    checkDivisibleBy100 =
      if divisibleBy 100 then
        No
      else
        Unsure

    checkDivisibleBy400 =
      if divisibleBy 400 then
        Yes
      else
        Unsure
\end{code}

Hmm... This is exactly what I did in 2nd iteration. But I thought in my head
and change the order of conditions to make a shortest path to reach the answer.
And maybe this implementation shows how I thought in my 2nd iteration.

Nevetheless, I don't really enjoy this approach because it is still not
general function or operator.

== Wrapping Up

We can make your own functions or operators, However if your operator is not
for general usage, It is generally good idea not to export to the module.

IMHO, Making an operator is only helpful when the operator gives more readable
form without making any confusion.

At the same time, Over-generalizing is always bad idea. It doesn't make
a problem simple, It doesn't make people understand code better, either.

Thank you for reading.
