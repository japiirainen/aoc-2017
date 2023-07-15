module Utils where

import IO
import IO.Base
import Data.Nat.Show as ℕ
import Data.Integer.Show as ℤ

open import Agda.Builtin.IO renaming (IO to PrimIO)
open import Data.List.Base using (List)
open import Data.String as String using (String)
open import Data.Nat.Base using (ℕ)
open import Data.Integer.Base using (ℤ)
open import Data.Maybe using (Maybe; just; nothing)

postulate
  getArgs_ : PrimIO (List String)

{-# FOREIGN GHC
import qualified System.Environment as Env
import qualified Data.Text as Text

getArgsText :: IO [Text.Text]
getArgsText = do { as <- Env.getArgs; return (map Text.pack as) }
#-}
{-# COMPILE GHC getArgs_ = getArgsText #-}

module CLI where
  open IO.Base

  getArgs : IO (List String)
  getArgs = lift getArgs_

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

Input : Set
Input = String

record Solution : Set where
  field
    part₁ : Input → String
    part₂ : Input → String

