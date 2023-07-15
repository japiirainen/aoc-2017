module Everything where

import Main
import Utils
import IO.Base

open import Function using (_$_)
open import Data.List.Base using (map; zip; upTo)
open import Data.Product.Base using (_×_; _,_)
open import Data.Nat.Base using (_+_)
open import Data.List.Base using ([]; _∷_)

solutions : Main.Solutions
solutions = map (λ (i , s) → 1 + i , s) $ zip (upTo 1) 
  []


module _ where
  open IO.Base using (Main)

  module Entry = Main.Entrypoint solutions

  main : Main
  main = IO.Base.run Entry.entrypoint
