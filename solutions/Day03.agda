{-# OPTIONS --guardedness --safe --cubical-compatible #-}

module Day03 where

open import Aoc

import Data.Tree.AVL.Map as MkMap

open import Function
open import Relation.Binary.PropositionalEquality using (refl; _≡_)
open import Relation.Binary.Bundles using (StrictTotalOrder)
open import Effect.Monad using (RawMonad)
open import Data.Nat.Base as ℕ using (ℕ; _∸_)
open import Data.Nat.Show using (readMaybe)
open import Data.Integer.Properties as Intₚ
open import Data.String using (String)
open import Data.Maybe.Base as Maybe using (Maybe; just; nothing; fromMaybe)
open import Data.Product.Base using (proj₁; proj₂; _×_; _,_)
open import Data.Product.Relation.Binary.Lex.Strict using (×-strictTotalOrder)
open import Data.List.Base as List using
  (List; _∷_; []; [_]; scanl; upTo; mapMaybe; sum; head;
   foldl; _++_; map; replicate; drop; length; take; dropWhileᵇ)
open import Data.List.Effectful as List
open import Data.Vec.Base as V using (Vec)

read-input : String → Maybe ℕ
read-input = readMaybe 10 ∘ strip

spiral : Coord → List Coord
spiral = accum movements
  where

  directions : List Movement
  directions = V.toList neigh₄

  moves : List Movement
  moves = List.concatMap (const directions) (upTo 300) -- 300 is an educated guess :-)

  open RawMonad List.monad

  movements : List Movement
  movements = do
    (d , n) ← List.zip moves (List.concatMap (replicate 2) (drop 1 (List.upTo (length moves ℕ.+ 1))))
    replicate n d

  accum : List Movement → Coord → List Coord
  accum xs c = scanl _|>_ c xs

module P₁ where

  dest : ℕ → Maybe Coord
  dest i = (take i $ spiral origin) ‼ (i ∸ 1)

  p₁ : ℕ → Maybe ℕ
  p₁ i = Maybe.map (manhattan origin) (dest i)

module P₂ where

  STO : StrictTotalOrder _ _ _
  STO = ×-strictTotalOrder Intₚ.<-strictTotalOrder Intₚ.<-strictTotalOrder

  open module Map = MkMap STO

  Sums = Map.Map ℕ

  sums : List ℕ
  sums = 1 ∷ List.map proj₂ (accum step (Map.singleton origin 1 , 1) (drop 1 (spiral origin)))
    where

    Acc : Set
    Acc = Sums × ℕ

    g : (List Acc → Coord → Acc) → List Acc → Coord → List Acc
    g f acc c = acc List.++ [ (f acc c) ]

    accum : (Acc → Coord → Acc) → Acc → List Coord → List Acc
    accum f init xs = go [ init ] xs
      where

      prev : List Acc → Acc
      prev xs = fromMaybe (Map.empty , 0) (xs ‼ (length xs ∸ 1))

      go : List Acc → List Coord → List Acc
      go as [] = as
      go as (c ∷ cs) = let acc = (f (prev as) c) in
        [ acc ] List.++ (go (as List.++ [ acc ]) cs)


    step : Acc → Coord → Acc
    step (m , _) c = (Map.insert c here m , here)
      where
      here : ℕ
      here = sum (mapMaybe (Map.lookup m) (V.toList (V.map (_$ c) neigh₈)))

  p₂ : ℕ → Maybe ℕ
  p₂ i = head $ dropWhileᵇ (ℕ._≤ᵇ i) sums

module _ where

  open P₁
  open P₂

  sol : Solution
  sol = let sol = λ f → fromMaybe "No solution." ∘ Maybe.map (show ∘ f) ∘ read-input
        in sol p₁ - sol p₂

  -- test on examples

  _ : (Solution.part₁ sol "1024") ≡ "31"
  _ = refl

  _ : (Solution.part₂ sol "800") ≡ "806"
  _ = refl
