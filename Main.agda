module Main where

import Aoc
import IO
import IO.Base
import IO.Finite
import System.Environment

open import Level
open import Function using (_$_)
open import Data.Unit.Polymorphic.Base using (⊤; tt)
open import Data.String.Base as String using (String)
open import Data.String.Properties using (_==_)
open import Data.Maybe using (Maybe; just; nothing; is-just)
open import Data.List.Base using (List; []; _∷_; length)
open import Data.Nat.Base using (ℕ)
open import Data.Nat.Show using (readMaybe)
open import Data.Product.Base using (_×_; _,_; proj₂)
open import Data.Bool using (if_then_else_)
open import Agda.Builtin.Nat as Nat

open Aoc

Solutions : Set
Solutions = List (ℕ × Solution)

module Entrypoint (solutions : Solutions) where
  open IO.Base using (_>>=_; _>>_; _<$>_; lift; IO)
  open IO.Finite using (putStrLn; readFile)
  open IO.List using (mapM′)
  open System.Environment using (getArgs)

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

  parse-args : List String → Args
  parse-args ("--day" ∷ day ∷ rest) = record (parse-args rest) { day = day:= (readMaybe 10 day) }
  parse-args ("--part" ∷ part ∷ rest) = record (parse-args rest) { part = parse-part part }
    where
      parse-part : String → Part
      parse-part "1" = one
      parse-part "2" = two
      parse-part _ = both
  parse-args _ = record { day = day:= nothing; part = both }

  run-day : Part → Input → Solution → IO {0ℓ} ⊤
  run-day one input (part₁ - _) = putStrLn $ "Part 1 : " String.++ (part₁ input)
  run-day two input (_ - part₂) = putStrLn $ "Part 2 : " String.++ (part₂ input)
  run-day both input s = run-day one input s >> run-day two input s

  find-solution : ℕ → Maybe Solution
  find-solution = go solutions
    where
      go : Solutions → ℕ → Maybe Solution
      go [] _ = nothing
      go ((i , s) ∷ ss) n = if i Nat.== n then just s else go ss n

  find-and-run : ℕ → Part → Input → IO {0ℓ} ⊤
  find-and-run day part input with (find-solution day)
  ... | just sol = run-day part input sol
  ... | nothing  = putStrLn $ "Day " String.++ (show day) String.++ " not implemented (yet)."

  path-for-day : ℕ → String
  path-for-day day = "./inputs/" String.++ (show day) String.++ ".txt"

  run : Args → IO {0ℓ} ⊤
  run args with (Args.day args)
  ... | day:= (just day) = do
    putStrLn $ "Running day : " String.++ (show day)
    input ← readFile $ path-for-day day
    find-and-run day (Args.part args) input
  ... | day:= nothing = do
      putStrLn $ "Running all " String.++ (show $ length solutions) String.++ " days"
      mapM′ go solutions
    where
      go : (ℕ × Solution) → IO {0ℓ} ⊤
      go (day , sol) = do
        putStrLn $ "---------- Day" String.++ (show day) String.++ " -------------"
        input ← readFile $ path-for-day day
        run-day (Args.part args) input sol

  -- entrypoint for aoc
  -- There are two main modes:
  -- 1. no arguments - run all days
  -- 2. args in format: --day 01 --part <1 OR 2>
  entrypoint : IO ⊤
  entrypoint = parse-args <$> getArgs >>= run

