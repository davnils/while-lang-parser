module Types where

type Varname = String

data Aexp
  = Numeral Integer
  | Variable Varname 
  | Aadd Aexp Aexp
  | Asub Aexp Aexp
  | Amul Aexp Aexp
  | Adiv Aexp Aexp
  deriving (Show, Eq)

data Bexp
  = Btrue
  | Bfalse
  | Beq Aexp Aexp
  | Bleq Aexp Aexp
  | Bneg Bexp
  | Band Bexp Bexp
  deriving (Show, Eq)

data Stm
  = Sass Varname Aexp 
  | Sskip
  | Scomp Stm Stm
  | Sif Bexp Stm Stm
  | Swhile Bexp Stm
  | Stry Stm Stm
  deriving (Show, Eq)
