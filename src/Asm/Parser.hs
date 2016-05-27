
-- Rivision 1

module Asm.Parser where

--import Control.Applicative hiding ((<|>), many)
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char

import Asm.Mnemonic

program :: Parser [Mnemonic]
program = do
  pg <- many1 $ line
  return pg

line :: Parser Mnemonic
line = do
  in1 <- instcode
  many1 endOfLine
  return in1

instcode :: Parser Mnemonic
instcode = do
  cd <- inst_add <|> inst_mov <|> inst_in <|> inst_out <|> inst_jump
  return cd

inst_add :: Parser Mnemonic
inst_add = do
  in1 <- string "add"
  many1 space
  rg1 <- register
  char ','
  im1 <- imdata
  return (toInst in1, (rg1, Just im1))

inst_mov :: Parser Mnemonic
inst_mov = do
  in1 <- string "mov"
  many1 space
  op  <- try (op_mov1) <|> (try op_mov2 <|> op_mov3)
  return (toInst in1, op)

inst_in :: Parser Mnemonic
inst_in = do
  in1 <- string "in"
  many1 space
  rg <- register
  return (toInst in1, (rg, Nothing))

inst_out :: Parser Mnemonic
inst_out = do
  in1 <- string "out"
  many1 space
  op <- regB <|> imdata
  return (toInst in1, (op, Nothing))

inst_jump :: Parser Mnemonic
inst_jump = do
  in1 <- try (string "jnc") <|> (string "jmp")
  many1 space
  im <- imdata
  return (toInst in1, (im, Nothing))

op_mov1 :: Parser (Operand, Maybe Operand)
op_mov1 = do
  rg <- register
  char ','
  im <- imdata
  return (rg, Just im)

op_mov2 :: Parser (Operand, Maybe Operand)
op_mov2 = do
  op1 <- regA
  char ','
  op2 <- regB
  return (op1, Just op2)

op_mov3 :: Parser (Operand, Maybe Operand)
op_mov3 = do
  op1 <- regB
  char ','
  op2 <- regA
  return (op1, Just op2)

register :: Parser Operand
register = do
  rg <- (regA <|> regB)
  return rg

regA :: Parser Operand
regA = do
  rg <- string "a"
  return $ RegA

regB :: Parser Operand
regB = do
  rg <- string "b"
  return $ RegB

imdata :: Parser Operand
imdata = do
  im <- count 4 (oneOf "01")
  return $ Imdata im

{-
eol :: Parser Char
eol = do
  e1 <- char '\r'
  e2 <- many char '\n'
  return e1
-}

-- support functions

toInst :: String -> Inst
toInst s = case s of
             "add" -> Add
             "mov" -> Mov
             "in"  -> In
             "out" -> Out
             "jnc" -> Jnc
             "jmp" -> Jmp

