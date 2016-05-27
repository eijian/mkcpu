

module Main where

import Text.Parsec

import Asm.Parser
import Asm.CodeGenerator

{-

-}

main :: IO ()
main = do
  pg <- getContents
  let mn = parse program "mnemonic" pg
      cd = generate mn
  mapM_ putStrLn cd


