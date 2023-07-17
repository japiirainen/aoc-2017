{-# OPTIONS --cubical-compatible --guardedness --safe #-}

module Day02 where

open import Aoc

open import Function
open import Level hiding (suc)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import Relation.Nullary using (does)
open import Effect.Monad using (RawMonad)
open import Effect.Applicative using (RawApplicativeZero)
open import Data.Bool.Base using (_∧_; not)
open import Data.List.Base using (List; _∷_; []; [_]; map; sum; head)
open import Data.List.Extrema.Nat using (min; max)
open import Data.List.Effectful as List
open import Data.Nat.Base using (ℕ; ∣_-_∣′; _/_; _+_; nonZero; suc; zero; _≡ᵇ_)
open import Data.Nat.Show using (readMaybe)
open import Data.Nat.Divisibility using (_∣?_)
open import Data.String as String using (String; lines; wordsByᵇ)
open import Data.Char using (isSpace)
open import Data.Maybe.Base as Maybe using (Maybe; just; nothing; fromMaybe; is-just)
open import Data.Maybe.Effectful as Maybe

open List.TraversableA {0ℓ} Maybe.applicative using (mapA)

Spreadsheet : Set
Spreadsheet = List (List ℕ)

read-input : String → Maybe Spreadsheet
read-input = mapA (mapA (readMaybe 10) ∘ (wordsByᵇ isSpace)) ∘ lines

checksum₁ : Spreadsheet → ℕ
checksum₁ = sum ∘ map (λ s → ∣ (max 0 s) - (min big s) ∣′)

checksum₂ : Spreadsheet → Maybe ℕ
checksum₂ = Maybe.map sum ∘ mapA ds
  where
  ds : List ℕ → Maybe ℕ
  ds xs = head do
            a ← xs
            b ← xs
            guard $ does (a ∣? b) ∧ not (a ≡ᵇ b)
            [ b div a ]

            where 
            open RawMonad List.monad
            open RawApplicativeZero List.applicativeZero

            _div_ : ℕ → ℕ → ℕ
            a div zero = 1
            a div (suc b) = _/_ a (suc b) {{nonZero}}

sol : Solution
sol = p₁ - p₂
  where
  p₁ = fromMaybe "No solution." ∘ Maybe.map (show ∘ checksum₁) ∘ read-input
  p₂ = fromMaybe "No solution." ∘ Maybe.map show ∘ (Maybe._>>= checksum₂) ∘ read-input

-- test on examples

_ : (Solution.part₁ sol "5 1 9 5\n7 5 3 \n2 4 6 8") ≡ "18"
_ = refl

_ : (Solution.part₂ sol "2 9 5 8\n9 4 7 3\n3 8 6 5") ≡ "9"
_ = refl
