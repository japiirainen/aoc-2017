{-# OPTIONS --cubical-compatible --guardedness --safe #-}

module Aoc where

import Data.Nat.Show as ℕ
import Data.Integer.Show as ℤ

open import Function
open import Data.List.Base
open import Data.String.Properties using (_≟_)
open import Data.String as String using (String; words)
open import Data.Nat.Base as ℕ using (ℕ)
open import Data.Char.Base using (isSpace)
open import Data.Integer.Base as ℤ using (ℤ; ∣_∣; +_; -_; 1ℤ; 0ℤ)
open import Data.Maybe using (Maybe; just; nothing)
open import Data.Product.Base using (_×_; _,_; proj₁; proj₂)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import Data.Vec.Base as V using (Vec)

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

  Show-List : {a : Set} → ⦃ Show a ⦄ → Show (List a)
  Show-List = show:= (String.unwords ∘ intersperse " , " ∘ map show)

  Show-× : {a b : Set} → ⦃ Show a ⦄ → ⦃ Show b ⦄ → Show (a × b)
  Show-× = show:= λ { (a , b) → "( " String.++ show a String.++ ") , " String.++ show b }

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
strip = String.fromList ∘ reverse ∘ dropWhileᵇ isSpace ∘ reverse ∘ dropWhileᵇ isSpace ∘ String.toList

_ : strip "  1 2 3   " ≡ "1 2 3"
_ = refl

big : ℕ
big = 100_000_000_000

module Coord where

  Coord = ℤ × ℤ

  origin : Coord
  origin = 0ℤ , 0ℤ

  manhattan : Coord → Coord → ℕ
  manhattan (p₁ , p₂) (q₁ , q₂) = ∣ p₁ ℤ.- q₁ ∣ ℕ.+ ∣ p₂ ℤ.- q₂ ∣

  _ : manhattan (+ 3 , + 2) (+ 67 , + 89) ≡ 151
  _ = refl

  Movement : Set
  Movement = Coord → Coord

  up down left right : Coord
  up = 0ℤ , - 1ℤ
  down = 0ℤ , 1ℤ
  left = - 1ℤ , 0ℤ
  right = 1ℤ , 0ℤ

  move : Coord → Coord → Coord
  move (x , y) (dx , dy) = x ℤ.+ dx , y ℤ.+ dy

  ⇓_ ⇑_ ⇒_ ⇐_ ⇖_ ⇗_ ⇙_ ⇘_ : Movement
  ⇓ from = move from down
  ⇑ from = move from up
  ⇒ from = move from right
  ⇐ from = move from left
  ⇖ from = move left (move from up)
  ⇗ from = move right (move from up)
  ⇙ from = move left (move from down)
  ⇘ from = move right (move from down)

  neigh₄ : Vec Movement 4
  neigh₄ = ⇒_ V.∷ ⇑_ V.∷ ⇐_ V.∷ ⇓_ V.∷ V.[]

  neigh₈ : Vec Movement 8
  neigh₈ = neigh₄ V.++ ⇘_ V.∷ ⇙_ V.∷ ⇗_ V.∷ ⇖_ V.∷ V.[]

open Coord public

module ListUtils where

  _!!_ : {A : Set} → List A → ℕ → Maybe A
  _!!_ = (head ∘ proj₂) ∘₂ flip splitAt

open ListUtils public
