{-# OPTIONS --cubical-compatible --guardedness --safe #-}

module Day01 where

open import Aoc

open import Level
open import Function
open import Data.List.Base hiding (fromMaybe)
open import Data.List.Effectful as List
open import Data.Nat.Base using (ℕ; _/_)
open import Data.Nat.Properties using (_≟_)
open import Data.String using (String; toList; fromChar; words)
open import Data.Nat.Show using (readMaybe)
open import Data.Maybe.Base as Maybe using (Maybe; just; fromMaybe)
open import Data.Maybe.Effectful as Maybe
open import Data.Product.Base using (_×_; _,_; uncurry; proj₁)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

Digits : Set
Digits = List ℕ

read-input : String → Maybe Digits
read-input = mapA (readMaybe 10) ∘ map fromChar ∘ toList
  where open List.TraversableA {0ℓ} Maybe.applicative using (mapA)

solve : ℕ → Digits → ℕ
solve n = sum ∘ map proj₁ ∘ filter (uncurry _≟_) ∘ pairs
  where 
  pairs : Digits → List (ℕ × ℕ)
  pairs xs = zip xs (drop n xs ++ xs)

sol : Solution
sol = p₁ - p₂
  where
  p₁ p₂ : PartSolution
  p₁ = fromMaybe "No solution" ∘ Maybe.map (show ∘ solve 1) ∘ read-input ∘ strip
  p₂ = fromMaybe "No solution" ∘ Maybe.map (show ∘ (λ xs → solve (length xs / 2) xs)) ∘ read-input ∘ strip

_ : Solution.part₁ sol "91212129" ≡ "9"
_ = refl

_ : Solution.part₂ sol "123123" ≡ "12"
_ = refl
