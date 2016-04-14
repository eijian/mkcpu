
module Main where

import Control.Concurrent
import System.Environment

import Logic.BasicGate
import Logic.FlipFlop
import Logic.ComplexLogic
import Logic.Register

wsec :: Int
wsec = 1                    -- wait 1 sec

defInput :: [Bin]
defInput  = toBits "0000"   -- default value of Input port

main :: IO ()
main = do
  inistat <- getContents
  opts <- getArgs
  let (wait, iport) = parseOpts opts
  loop lc_counter4 (toBits inistat) wait

parseOpts :: [String] -> (Int, [Bin])
parseOpts [] = (wsec, defInput)
parseOpts (x:zs) = ((read :: String -> Int) x, defInput)

loop :: LogicCircuit -> [Bin] -> Int -> IO ()
loop lc is w = do
  let os = lc ([sHI, sHI] ++ is ++ toBits "0000")
  putStatus os
  threadDelay (w * 1000 * 1000)
  loop lc os w



putStatus :: [Bin] -> IO ()
putStatus os = putStrLn $ toStr os

