-- This module serves as the root of the `Feline` library.
-- Import modules here that should be built as part of the library.
import Feline.Basic
import Feline.Pos

import LeanCopilot  

example (a b c : Nat) : a + b + c = a + c + b := by
  suggest_tactics
