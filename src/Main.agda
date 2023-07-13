module Main where

import Utils
import IO
import IO.Base
import IO.Finite

open import Level
open import Data.Unit.Polymorphic.Base using (⊤)
open import Data.String.Base as String using (String)
open import Data.String.Properties using (_==_)
open import Data.Maybe using (Maybe; just; nothing)
open import Data.List.Base using (List; []; _∷_)
open import Data.Nat.Base using (ℕ)
open import Data.Nat.Show using (readMaybe)

module Entrypoint where
  open IO.Base using (Main; _>>=_; _>>_; _<$>_; lift; run; IO)
  open IO.Finite using (putStrLn)
  open IO.List using (mapM′)
  open Utils
  open Utils.CLI

  data Part : Set where
    one  : Part
    two  : Part
    both : Part

  instance
    Show-Part : Show Part
    Show-Part = show:= λ where
      one  → "part 1"
      two  → "part 2"
      both → "both parts"

  record Day : Set where
    constructor day:=
    field
      -- should really be (Fin 25)
      day : Maybe ℕ

  instance
    Show-Day : Show Day
    Show-Day = show:= λ where
        (day:= d) → show-d d
      where
        show-d : Maybe ℕ → String
        show-d (just x) = "day " String.++ (show x)
        show-d nothing = "all days"

  record Args : Set where
    field
      day : Day
      part : Part

  instance
    Show-Args : Show Args
    Show-Args = show:= λ p →
      show (Args.day p) String.++ " - " String.++ show (Args.part p)

  parseArgs : List String → Args
  parseArgs ("--day" ∷ day ∷ rest) = record (parseArgs rest) { day = day:= (readMaybe 10 day) }
  parseArgs ("--part" ∷ part ∷ rest) = record (parseArgs rest) { part = parsePart part }
    where
      parsePart : String → Part
      parsePart "1" = one
      parsePart "2" = two
      parsePart _ = both
  parseArgs _ = record { day = day:= nothing; part = both }

  -- entrypoint for aoc
  -- There are two main modes:
  -- 1. no arguments - run all days
  -- 2. args in format: --day 01 --part <1 OR 2>
  entrypoint : IO ⊤
  entrypoint = do
    args ← parseArgs <$> getArgs
    -- TODO: use args to choose what to run
    putStrLn (show args)

  main : Main
  main = run entrypoint
