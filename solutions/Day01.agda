{-# OPTIONS --cubical-compatible --guardedness --safe #-}

module Day01 where

open import Function using (id)
open import Aoc using (Solution; PartSolution; _-_)

sol : Solution
sol = p₁ - p₂
  where
    p₁ p₂ : PartSolution
    p₁ = id
    p₂ = id
