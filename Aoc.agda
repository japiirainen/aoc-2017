{-# OPTIONS --cubical-compatible --guardedness #-}

module Aoc where

import Data.Nat.Show as ℕ
import Data.Integer.Show as ℤ

open import Data.List.Base using (List)
open import Data.String as String using (String)
open import Data.Nat.Base using (ℕ)
open import Data.Integer.Base using (ℤ)
open import Data.Maybe using (Maybe; just; nothing)

record Show (a : Set) : Set where
  constructor show:=
  field
    show : a → String

open Show ⦃...⦄ public

instance
  Show-ℕ : Show ℕ
  Show-ℕ = show:= ℕ.show

  Show-ℤ : Show ℤ
  Show-ℤ = show:= ℤ.show

  Show-Maybe : {a : Set} → ⦃ Show a ⦄ → Show (Maybe a)
  Show-Maybe = show:= λ where
    nothing → "nothing"
    (just x) → show x

Input : Set
Input = String

PartSolution : Set
PartSolution = Input → String

record Solution : Set where
  constructor _-_
  field
    part₁ : PartSolution
    part₂ : PartSolution

