{-# OPTIONS --cubical-compatible --guardedness --safe #-}

module Aoc where

import Data.Nat.Show as ℕ
import Data.Integer.Show as ℤ

open import Function
open import Data.List.Base
open import Data.String.Properties using (_≟_)
open import Data.String as String using (String; words)
open import Data.Nat.Base as ℕ using (ℕ; _<ᵇ_; _+_)
open import Data.Nat.Show as ℕ using ()
open import Data.Char.Base using (isSpace)
open import Data.Integer.Base as ℤ using (ℤ; ∣_∣; +_; -_; 1ℤ; 0ℤ)
open import Data.Maybe as Maybe using (Maybe; just; nothing)
open import Data.Product.Base using (_×_; _,_; proj₁; proj₂)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import Data.Vec.Base as V using (Vec)

record Show (a : Set) : Set where
  constructor show:=
  field
    show : a → String

open Show ⦃...⦄ public

readℤ : String → Maybe ℤ
readℤ s with String.toList s
... | '-' ∷ rest = Maybe.map (ℤ.-_ ∘ ℤ.+_) (ℕ.readMaybe 10 (String.fromList rest))
... | '+' ∷ rest = Maybe.map ℤ.+_ (ℕ.readMaybe 10 (String.fromList rest))
... | _ = Maybe.map ℤ.+_ (ℕ.readMaybe 10 s)

_ : readℤ "+10" ≡ just (ℤ.+ 10)
_ = refl

_ : readℤ "-10" ≡ just (ℤ.- (ℤ.+ 10))
_ = refl

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

  Show-Vec : {n : ℕ} → {a : Set} → ⦃ Show a ⦄ → Show (Vec a n)
  Show-Vec = show:= (show ∘ V.toList)

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

  _‼_ : {A : Set} → List A → ℕ → Maybe A
  _‼_ = (head ∘ proj₂) ∘₂ flip splitAt

  _ℤ-‼_ : {A : Set} → List A → ℤ → Maybe A
  [] ℤ-‼ _ = nothing
  _ ℤ-‼ ℤ.negsuc n = nothing
  (x ∷ xs) ℤ-‼ ℤ.+_ ℕ.zero = just x
  (_ ∷ xs) ℤ-‼ ℤ.+_ (ℕ.suc n) = xs ℤ-‼ (ℤ.+ n)

  _ℤ[_]%=_ : {A : Set} → List A → ℤ → (A → A) → List A
  [] ℤ[ _ ]%= _ = []
  (x ∷ xs) ℤ[ ℤ.+_ ℕ.zero ]%= f = f x ∷ xs
  (x ∷ xs) ℤ[ ℤ.+_ (ℕ.suc n) ]%= f = x ∷ (xs ℤ[ (ℤ.+ n) ]%= f)
  (x ∷ xs) ℤ[ ℤ.negsuc n ]%= f = x ∷ xs

  _ℕ[_]%=_ : {A : Set} (xs : List A) → ℕ → (A → A) → List A
  [] ℕ[ ℕ.zero ]%= f = []
  [] ℕ[ ℕ.suc n ]%= f = []
  (x ∷ xs) ℕ[ ℕ.zero ]%= f = f x ∷ xs
  (x ∷ xs) ℕ[ ℕ.suc n ]%= f =  x ∷ (xs ℕ[ n ]%= f)

  _∙∙∙_ : ℕ → ℕ → List ℕ
  lo ∙∙∙ hi = dropWhileᵇ (λ n → n <ᵇ lo) $ upTo (hi + 1)


open ListUtils public
