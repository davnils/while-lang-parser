--------------------------------------------------------------------
-- |
-- Module : Language.While.Types
--
-- Provides shared type definitions forming the parsed AST.
--

module Language.While.Types where

-- | Variables type alias.
type Varname = String

-- | Arithmetic expressions, including integer division.
data Aexp
  = Numeral Integer
  | Variable Varname 
  | Aadd Aexp Aexp
  | Asub Aexp Aexp
  | Amul Aexp Aexp
  | Adiv Aexp Aexp
  deriving (Show, Eq)


-- | Boolean expressions.
data Bexp
  = Btrue
  | Bfalse
  | Beq Aexp Aexp
  | Bleq Aexp Aexp
  | Bneg Bexp
  | Band Bexp Bexp
  deriving (Show, Eq)

-- | Statements, including try-catch clauses.
data Stm
  = Sass Varname Aexp 
  | Sskip
  | Scomp Stm Stm
  | Sif Bexp Stm Stm
  | Swhile Bexp Stm
  | Stry Stm Stm
  deriving (Show, Eq)
