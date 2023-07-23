{-# OPTIONS --safe --guardedness --cubical-compatible #-}

module Day04 where

import Data.Tree.AVL.Sets as STO→Set
import Data.List.Sort.MergeSort as DTO→Sort
import Data.Char as Char

open import Aoc
open import Function
open import Data.List using (List; _∷_; []; map; filter; length; foldl)
open import Data.String as String using (String; lines; wordsBy)
open import Data.String.Properties as Stringᵖ
open import Data.Nat as ℕ using (ℕ)
open import Relation.Binary.PropositionalEquality using (refl; _≡_)

Words : Set
Words = List String

read-input : String → List Words
read-input = map (wordsBy (Char._≟ ' ')) ∘ lines

open STO→Set Stringᵖ.<-strictTotalOrder-≈ using (⟨Set⟩; fromList; size)

unique : Words → ⟨Set⟩
unique = fromList

unique-%-order : Words → ⟨Set⟩
unique-%-order = let module Sort = DTO→Sort Stringᵖ.≤-decTotalOrder-≈ in
  unique ∘ map (foldl String._++_ "" ∘ Sort.sort ∘ map String.fromChar ∘ String.toList)
  
solve : (Words → ⟨Set⟩) → List Words → ℕ
solve f = length ∘ filter λ ws → length ws ℕ.≟ size (f ws)

sol : Solution
sol = let sol = λ f → show ∘ f ∘ read-input in
      sol (solve unique) - sol (solve unique-%-order)

_ : Solution.part₁ sol "aa bb cc dd ee\naa bb cc dd aa\naa bb cc dd aaa" ≡ "2"
_ = refl

_ : Solution.part₂ sol "abcde fghij\nabcde xyz ecdab\na ab abc abd abf abj\niiii oiii ooii oooi oooo\noiii ioii iioi iiio" ≡ "3"
_ = refl

