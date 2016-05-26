
module Asm.Mnemonic where

{-
  MNEMONIC OF TD4


-}

data Inst = Add | Mov | In | Out | Jnc | Jmp deriving (Enum, Show)
data Operand = RegA | RegB | Imdata String deriving Show

