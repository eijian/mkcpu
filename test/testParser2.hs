
module Main where

import Text.Parsec

import Asm.Mnemonic
import Asm.Parser

main :: IO ()
main = do
  parseTest inst_add "add a,0110"
  parseTest inst_add "add a,b"
  parseTest inst_add "add b,1100"
  parseTest inst_add "add b,a"
  parseTest inst_add "add a,a"

  parseTest inst_mov "mov a,b"
  parseTest inst_mov "mov b,a"
  parseTest inst_mov "mov a,a"
  parseTest inst_mov "mov b,b"
  parseTest inst_mov "mov a,0011"
  parseTest inst_mov "mov b,0011"
  parseTest inst_mov "mov 0011,b"
  parseTest inst_mov "mov 0011,0110"

  parseTest inst_in  "in a"
  parseTest inst_in  "in b"
  parseTest inst_in  "in 1100"

  parseTest inst_out "out b"
  parseTest inst_out "out 1010"
  parseTest inst_out "out a"
  
  parseTest inst_jump "jnc 1110"
  parseTest inst_jump "jnc a"
  parseTest inst_jump "jnc b"

  parseTest inst_jump "jmp 1110"
  parseTest inst_jump "jmp a"
  parseTest inst_jump "jmp b"

  parseTest program "add a,1100\r\njmp 1100\r\n"
  parseTest program "add a,1100\njmp 1100\n"
  parseTest program "\r\nadd a,1100\r\njmp 1100\r\n"
  parseTest program "add a,1100\r\njmp 1100\r\n\r\n"
  parseTest program "add a,1100 jmp 1100\r\n"

  putStrLn "fin"



