
module Main where

import Control.Applicative hiding ((<|>))
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char

data Inst = Add | Mov | In | Out | Jnc | Jmp deriving (Enum, Show)
data Operand = RegA | RegB | Imdata String deriving Show

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
  parseTest code2 "add a,b"
  parseTest code2 "mov a,0011"
  parseTest code2 "mov   a,0011"
  parseTest code2 "in a"
  parseTest code2 "jmp a,b"
  parseTest code1 "jmp a"
  parseTest code1 "jmp 1011"
  parseTest code1 "add a"
  parseTest code1 "jnc 1100"
  parseTest code1 "in  b"
  parseTest code1 "in  0110"
  parseTest instcode "add a,b\n"
  parseTest instcode "add a,b\r\n"
  parseTest instcode "add a,b"
  parseTest instcode "jmp 1100\r\n"
  parseTest program "add a,1100\rjmp 1100\r\n"
  parseTest program "add a,1100 jmp 1100\r\n"
  putStrLn "fin"



