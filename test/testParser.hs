
module Main where

import Control.Applicative hiding ((<|>))
import Data.Char
import Text.Parsec
import Text.Parsec.String

data Inst = Add | Mov | In | Out | Jnc | Jmp deriving (Enum, Show)
data Operand = RegA | RegB | Imdata String deriving Show


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

main :: IO ()
main = do
  parseTest inst2 "add"
  parseTest inst2 "mov"
  parseTest inst2 "abc"
  parseTest inst1 "in"
  parseTest inst1 "out"
  parseTest inst1 "jnc"
  parseTest inst1 "jmp"
  parseTest inst1 "aaa"
  parseTest inst1 "jaa"
  parseTest register "a"
  parseTest register "b"
  parseTest register "c"
  parseTest imdata "0100"
  parseTest imdata "10100"
  parseTest imdata "1012"
  parseTest operand1 "1100"
  parseTest operand1 "a"
  parseTest operand1 "aa"
  parseTest operand2 "a,b"
  parseTest operand2 "b,a"
  parseTest operand2 "a,0101"
  parseTest operand2 "b,1100"
  parseTest operand2 "c,1100"
  parseTest operand2 "a 1100"
  parseTest operand2 "a,1200"
  parseTest operand2 "aa,1100"
  putStrLn "fin"



