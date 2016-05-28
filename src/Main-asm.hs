

module Main where

--import Control.Monad
import Text.Parsec

import Asm.Parser
import Asm.CodeGenerator

{-

-}

main :: IO ()
main = do
  pg <- getContents

  let mn = parse program "TD4 asm" pg
      cd = generate mn
  mapM_ putStrLn cd
{-
  -- this code outputs error at the end of source file.
  let ls = lines pg
  forM_ ls $ \i -> do
    let cd = parse instcode "TD4 asm" i
    putStrLn $ generateOne cd
-}

