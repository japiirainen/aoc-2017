{-# OPTIONS --safe --guardedness --cubical-compatible #-}

-- Day 5 ended up being embarassingly slow... 
-- We would need some kind of mutable vectors / arrays to make this
-- efficient

module Day05 where

import Data.List.Effectful as List
import Data.Maybe.Effectful as Maybe

open import Aoc

open import Level
open import Function
open import Data.String as String using (String)
open import Data.List using (List; map; _∷_; [])
open import Data.Bool using (if_then_else_)
open import Data.Nat as ℕ using (ℕ; _+_; suc; zero)
open import Data.Integer as ℤ using (ℤ; 0ℤ; 1ℤ)
open import Algebra.Core using (Op₂)
open import Data.Maybe as Maybe using (Maybe; fromMaybe; just; nothing)
open import Data.Product using (_×_; _,_; proj₁)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

read-input : String → Maybe (List ℤ)
read-input =  mapA readℤ ∘ String.lines ∘ strip
  where open List.TraversableA {0ℓ} Maybe.applicative

solve : (Op₂ ℤ) → List ℤ → Maybe ℕ
solve f = go 0 0ℤ big
  where
  go : ℕ → ℤ → ℕ → List ℤ → Maybe ℕ
  go steps idx zero xs = nothing
  go steps idx (suc bound-1) xs with xs ℤ-‼ idx
  ... | nothing = just steps
  ... | just offset = 
    go (steps + 1) (idx ℤ.+ offset) bound-1 (xs ℤ[ idx ]%= (f offset))

sol : Solution
sol = let go f = fromMaybe "No solution." ∘  Maybe.map show ∘ (Maybe._>>= solve f) ∘ read-input in
  go rule₁ - go rule₂
    where
    rule₁ rule₂ : Op₂ ℤ
    rule₁ _ e = e ℤ.+ 1ℤ
    rule₂ offset e = if (offset ℤ.≤ᵇ (ℤ.+ 2)) then (e ℤ.+ 1ℤ) else (e ℤ.- 1ℤ)

sample : String
sample = "0\n3\n0\n1\n-3\n"

_ : Solution.part₁ sol sample ≡ "5"
_ = refl

_ : Solution.part₂ sol sample ≡ "10"
_ = refl
