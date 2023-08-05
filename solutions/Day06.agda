{-# OPTIONS --guardedness --cubical-compatible --sized-types #-}

module Day06 where

import Data.List.Effectful as List
import Data.List.Extrema as TO→Extrema
import Data.Maybe.Effectful as Maybe
import Data.Nat.Properties as ℕᵖ
import Data.Tree.AVL.Map as STO→Map
import Data.List.Relation.Binary.Lex.Strict as Listᵖ

open import Aoc
open import Function
open import Size using (∞)
open import Codata.Sized.Thunk as Thunk using (Thunk; force)
open import Effect.Monad using (RawMonad)
open import Level renaming (suc to lsuc) 
open import Data.String as String using (String; wordsByᵇ)
open import Data.Char as Char using (isSpace)
open import Data.List using (List; _∷_; []; [_]; length; zip; upTo; map; dropWhileᵇ; foldl)
open import Data.Bool using (if_then_else_)
open import Data.Nat using (ℕ; _∸_; _/_; _%_; _*_; _+_; _≡ᵇ_; suc; zero; _<ᵇ_)
open import Data.Nat.Show using (readMaybe)
open import Data.Maybe as Maybe using (Maybe; fromMaybe; just; nothing)
open import Data.Product.Base using (proj₁; proj₂; _,_; _×_)
open import Codata.Sized.Stream using (Stream; iterate)
open import Relation.Binary using (StrictTotalOrder)
open import Relation.Binary.PropositionalEquality using (refl; _≡_)

Banks : Set
Banks = List ℕ

read-banks : String → Maybe Banks
read-banks = mapA (readMaybe 10) ∘ wordsByᵇ isSpace ∘ strip
  where open List.TraversableA {0ℓ} Maybe.applicative

module Step where

  open TO→Extrema ℕᵖ.≤-totalOrder using (argmax)

  step : Banks → Banks
  step xs = accum (map (λ (x , j) → if i ≡ᵇ j then 0 else x) indexed) ss
    where
    n = length xs
    indexed = zip xs (upTo n)
    max,i = argmax proj₁ (0 , 0) indexed
    max = proj₁ max,i
    i = proj₂ max,i

    open RawMonad List.monad

    accum : Banks → List (ℕ × ℕ) → Banks
    accum bs s = foldl (λ b (ix , n) → b ℕ[ ix ]%= (_+ n)) bs s

    _rem_ : ℕ → ℕ → ℕ
    j rem n with n
    ... | zero = 0
    ... | s@(suc _) = j % s

    r = (i + 1) ∙∙∙ (i + max)

    ss : List (ℕ × ℕ)
    ss = r >>= λ j → [ j rem n , 1 ]
      
  _ : step (0 ∷ 2 ∷ 7 ∷ 0 ∷ []) ≡ 2 ∷ 4 ∷ 1 ∷ 2 ∷ []
  _ = refl

  _ : step (step (0 ∷ 2 ∷ 7 ∷ 0 ∷ [])) ≡ 3 ∷ 1 ∷ 2 ∷ 3 ∷ []
  _ = refl

module Loop {a ℓ₁ ℓ₂} (sto : StrictTotalOrder a ℓ₁ ℓ₂) where

  open StrictTotalOrder sto renaming (Carrier to Key)

  open STO→Map sto

  find-loop : Stream Key ∞ → ℕ × ℕ
  find-loop = go empty big
    where
    go : Map ℕ → ℕ → Stream Key ∞ → ℕ × ℕ
    go seen zero _ = 69 , 69
    go seen (suc r) (x Stream.∷ xs) with lookup seen x
    ... | nothing = let n = size seen in go (insert x n seen) r (Thunk.force xs)
    ... | (just i) = let n = size seen in n , n ∸ i

module Solve where

  open Step using (step) 

  open Loop (Listᵖ.<-strictTotalOrder ℕᵖ.<-strictTotalOrder) using (find-loop)

  solve : Banks → ℕ × ℕ
  solve = find-loop ∘ iterate step

open Solve

sol : Solution
sol = let go = λ f → fromMaybe "No solution." ∘ Maybe.map (show ∘ f ∘ solve) ∘ read-banks in
  go proj₁ - go proj₂

_ : Solution.part₁ sol "0\t2\t7\t0\t" ≡ "5"
_ = refl

_ : Solution.part₂ sol "0\t2\t7\t0\t" ≡ "4"
_ = refl
