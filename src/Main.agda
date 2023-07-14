module Main where

import Utils
import IO
import IO.Base
import IO.Finite

open import Level
open import Function using (_$_)
open import Data.Unit.Polymorphic.Base using (⊤; tt)
open import Data.String.Base as String using (String)
open import Data.String.Properties using (_==_)
open import Data.Maybe using (Maybe; just; nothing; is-just)
open import Data.List.Base using (List; []; _∷_; map; upTo; zip; length)
open import Data.Nat.Base using (ℕ)
open import Data.Nat.Show using (readMaybe)
open import Data.Product.Base using (_×_; _,_; proj₂)
open import Data.Bool using (if_then_else_)
open import Agda.Builtin.Nat as Nat

module Entrypoint where
  open IO.Base using (Main; _>>=_; _>>_; _<$>_; lift; IO)
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

  Solutions : Set
  Solutions = List (ℕ × Solution)

  solutions : Solutions
  solutions = map (λ (i , s) → 1 + i , s) $ zip (upTo 1) []

  runDay : {ℓ : Level} → Part → Solution → IO {ℓ} ⊤
  runDay one s = putStrLn $ "Part 1 : " String.++ (Solution.part₁ s tt)
  runDay two s = putStrLn $ "Part 2 : " String.++ (Solution.part₂ s tt)
  runDay both s = runDay one s >> runDay two s

  findSolution : ℕ → Maybe Solution
  findSolution = go solutions
    where
      go : Solutions → ℕ → Maybe Solution
      go [] _ = nothing
      go ((i , s) ∷ ss) n = if i Nat.== n then just s else go ss n

  findAndRun : {ℓ : Level} → ℕ → Part → IO {ℓ} ⊤
  findAndRun day part with (findSolution day)
  ... | just sol = runDay part sol
  ... | nothing  = putStrLn $ "Day " String.++ (show day) String.++ " not implemented (yet)."

  run : {ℓ : Level} → Args → IO {ℓ} ⊤
  run args with (Args.day args)
  ... | day:= (just day) = do
    putStrLn $ "Running day : " String.++ (show day)
    findAndRun day (Args.part args)
  ... | day:= nothing = do
    putStrLn $ "Running all " String.++ (show $ length solutions) String.++ " days"
    mapM′ (λ (i , d) →
             (putStrLn $ "---------- Day" String.++ (show i) String.++ " -------------")
             >> runDay (Args.part args) d) solutions

  -- entrypoint for aoc
  -- There are two main modes:
  -- 1. no arguments - run all days
  -- 2. args in format: --day 01 --part <1 OR 2>
  entrypoint : IO ⊤
  entrypoint = parseArgs <$> getArgs >>= run

  main : Main
  main = IO.Base.run entrypoint
