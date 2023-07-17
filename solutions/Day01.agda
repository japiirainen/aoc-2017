{-# OPTIONS --cubical-compatible --guardedness --safe #-}

module Day01 where

open import Aoc using (Solution; PartSolution; _-_)

open import Level
open import Function using (id; _∘_; _$_; const)
open import Data.List.Base
open import Data.List.Effectful as List
open import Data.Nat.Base using (ℕ; _/_)
open import Data.Nat.Properties using (_≟_)
open import Data.String using (String; toList; fromChar; words)
open import Data.Nat.Show using (readMaybe)
open import Data.Maybe.Base as Maybe using (Maybe; just)
open import Data.Maybe.Effectful as Maybe
open import Data.Product.Base using (_×_; _,_; uncurry; proj₁)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

Digits : Set
Digits = List ℕ

read-input : String → Maybe Digits
read-input = mapA (readMaybe 10) ∘ map fromChar ∘ toList
  where open List.TraversableA {0ℓ} Maybe.applicative using (mapA)

_ : read-input "1122" ≡ just (1 ∷ 1 ∷ 2 ∷ 2 ∷ [])
_ = refl

sample : Maybe Digits
sample = read-input "1122"

solve : ℕ → Digits → ℕ
solve n = sum ∘ map proj₁ ∘ filter (uncurry _≟_) ∘ pairs
  where 
  pairs : Digits → List (ℕ × ℕ)
  pairs xs = zip xs (drop n xs ++ xs)

_ : solve 1 (9 ∷ 1 ∷ 1 ∷ 2 ∷ 2 ∷ 9 ∷ []) ≡ 12
_ = refl

_ : solve 2 (1 ∷ 2 ∷ 1 ∷ 2 ∷ []) ≡ 6
_ = refl

sol : Solution
sol = p₁ - p₂
  where
    open Aoc
    p₁ p₂ : PartSolution
    p₁ = show ∘ Maybe.map (solve 1) ∘ read-input ∘ strip
    p₂ = show ∘ Maybe.map (λ xs → solve (length xs / 2) xs) ∘ read-input ∘ strip
