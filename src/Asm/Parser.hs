
module Asm.Parser where

import Control.Applicative hiding ((<|>))
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char

import Asm.Mnemonic

program :: Parser [(Inst, (Operand, Maybe Operand))]
program = do
  pg <- many1 $ instcode
  return pg

instcode :: Parser (Inst, (Operand, Maybe Operand))
instcode = do
  cd <- code2 <|> code1
  many1 $ oneOf "\r\n"
  return cd

code2 :: Parser (Inst, (Operand, Maybe Operand))
code2 = do
  in2 <- inst2
  many1 space
  op2 <- operand2
  return (in2, op2)

code1 :: Parser (Inst, (Operand, Maybe Operand))
code1 = do
  in1 <- inst1
  many1 space
  op1 <- operand1
  return (in1, (op1, Nothing))

inst2 :: Parser Inst
inst2 = do
  i2 <- (string "add" <|> string "mov")
  let i = if i2 == "add" then Add else Mov
  return i

inst1 :: Parser Inst
inst1 = do
  i1 <- (string "in" <|> string "out" <|>
         try (string "jnc") <|> (string "jmp"))
  let i = case i1 of
            "in"  -> In
            "out" -> Out
            "jnc" -> Jnc
            "jmp" -> Jmp
  return i

operand2 :: Parser (Operand, Maybe Operand)
operand2 = do
  op2 <- register
  char ','
  op1 <- operand1
  return $ (op2, Just op1)

operand1 :: Parser Operand
operand1 = do
  op1 <- (register <|> imdata)
  return op1

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




