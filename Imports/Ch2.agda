{-# OPTIONS --safe --cubical-compatible #-}

module Imports.Ch2 where

-- The names and types from Ch0 differ from those in the Agda stdlib.
-- Soon we will redefine these functions to match the stdlib.
-- import Function
open import Solutions.Ch0 using (id; const; compose; apply; flip; on; lift) public

-- import Data.Bool
open import Solutions.Ch1 using (Bool; false; true; not; _∧_; _∨_; _xor_; _&&_; _||_; if_then_else_) public

{-

Here we "open" these modules "using" a set of identifiers.

This means that, while the entire module is imported, only the listed identifiers are in the global namespace.

The rest must be opened separately or accessed through the module, i.e. "Solutions.Ch1.Predicate".

Furthermore, "public" causes these symbols to be re-exported as part of this module.

From now on, above each import you will see a comment containing the Agda stdlib module with the same contents.

-}