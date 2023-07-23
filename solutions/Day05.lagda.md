## Day 5

Let's start with the usual module headers and import needed dependencies.

```agda
{-# OPTIONS --guardedness --safe --cubical-compatible #-}

module Day05 where

open import Aoc

open import Function
```

Finally tie it all together to a `Solution`.

```agda
sol : Solution
sol = id - id
```
