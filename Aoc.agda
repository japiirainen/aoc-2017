{-# OPTIONS --cubical-compatible --guardedness --safe #-}

module Aoc where

import Data.Nat.Show as ℕ
import Data.Integer.Show as ℤ

open import Function
open import Data.List.Base
open import Data.String.Properties using (_≟_)
open import Data.String as String using (String; words)
open import Data.Nat.Base using (ℕ)
open import Data.Integer.Base using (ℤ)
open import Data.Maybe using (Maybe; just; nothing)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

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

strip : String → String
strip = String.intersperse " " ∘
        reverse ∘
        dropWhile (_≟ "") ∘
        reverse ∘
        dropWhile (_≟ "") ∘
        words

_ : strip "  1 2 3   " ≡ "1 2 3"
_ = refl
