{-# OPTIONS --safe --guardedness --cubical-compatible #-}

module Day05 where

import Data.List.Effectful as List
import Data.Maybe.Effectful as Maybe
import Data.Tree.AVL.Map as MkMap

open import Aoc

open import Level
open import Function
open import Data.String as String using (String)
open import Data.List using (List; map; _∷_; []; zip; length; upTo)
open import Data.Bool using (if_then_else_)
open import Data.Nat as ℕ using (ℕ; _+_; suc; zero)
open import Data.Integer as ℤ using (ℤ; 0ℤ; 1ℤ)
open import Data.Integer.Properties as ℤᵖ
open import Algebra.Core using (Op₂)
open import Data.Maybe as Maybe using (Maybe; fromMaybe; just; nothing; maybe)
open import Data.Product using (_×_; _,_; proj₁)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

read-input : String → Maybe (List ℤ)
read-input = mapA readℤ ∘ String.lines ∘ strip
  where open List.TraversableA {0ℓ} Maybe.applicative

open MkMap ℤᵖ.<-strictTotalOrder using (Map; fromList; member; insertWith; lookup)

solve : (Op₂ ℤ) → List ℤ → Maybe ℕ
solve f xs = go big 0 0ℤ (fromList $ zip (map (ℤ.+_) $ upTo $ length xs) xs)
  where
  go : ℕ → ℕ → ℤ → Map ℤ → Maybe ℕ
  go zero _ _ _ = nothing
  go (suc b-1) steps idx m with lookup m idx
  ... | nothing = just steps
  ... | just offset = go b-1 (steps + 1) (idx ℤ.+ offset) (insertWith idx (maybe (f offset) 0ℤ) m)

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
