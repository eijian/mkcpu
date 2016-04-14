
module Main where

import Control.Concurrent

import Logic.BasicGate
import Logic.FlipFlop
import Logic.ComplexLogic
import Logic.Register

wsec = 1 :: Int  -- wait 1 sec
inistat = toBits "0000" :: [Bin]

main :: IO ()
main = loop lc_counter4 inistat

loop :: LogicCircuit -> [Bin] -> IO ()
loop lc is = do
  let os = lc ([sHI, sHI] ++ is ++ toBits "0000")
  putStatus os
  threadDelay (wsec * 1000 * 1000)
  loop lc os

putStatus :: [Bin] -> IO ()
putStatus os = putStrLn $ toStr os

