{-# OPTIONS --guardedness --sized-types #-}

module Everything where

import Aoc
import Main
import IO.Base

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07

open import Function using (_$_; _∘_)
open import Data.List.Base using (List; map; zip; upTo; length)
open import Data.Product.Base using (_×_; _,_; map₁)
open import Data.Nat.Base using (_+_; suc)
open import Data.List.Base using ([]; _∷_)

open Main

ss : List Aoc.Solution
ss = Day01.sol ∷
     Day02.sol ∷
     Day03.sol ∷
     Day04.sol ∷
     Day05.sol ∷
     Day06.sol ∷
     Day07.sol ∷
     []

⇒Solutions : (List Aoc.Solution) → Solutions
⇒Solutions = map (map₁ suc) ∘ zip (upTo $ length ss)

module _ where

  module Entry = Entrypoint (⇒Solutions ss)

  open IO.Base

  main : Main
  main = run Entry.entrypoint

