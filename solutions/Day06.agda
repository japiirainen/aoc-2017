{-# OPTIONS --safe --guardedness --cubical-compatible #-}

module Day06 where

import Data.List.Effectful as List
import Data.Maybe.Effectful as Maybe

open import Aoc

open import Level
open import Function
open import Data.String as String using (String; wordsByᵇ)
open import Data.Char as Char using (isSpace)
open import Data.List using (List)
open import Data.Nat using (ℕ)
open import Data.Nat.Show using (readMaybe)
open import Data.Maybe as Maybe using (Maybe; fromMaybe)

read-input : String → Maybe (List ℕ)
read-input = mapA (readMaybe 10) ∘ wordsByᵇ isSpace ∘ strip
  where open List.TraversableA {0ℓ} Maybe.applicative

sol : Solution
sol = let go = fromMaybe "No solution." ∘ Maybe.map show ∘ read-input in
  go - go

sample : String
sample = "0\t2\t7\t0"
