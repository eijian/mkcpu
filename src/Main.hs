
module Main where

import Control.Concurrent
import System.Environment

import Logic.BasicGate
import Logic.FlipFlop
import Logic.ComplexLogic
import Logic.Register

wsec :: Float
wsec = 1                    -- wait 1 sec

defInput :: [Bin]
defInput  = toBits "0000"   -- default value of Input port

main :: IO ()
main = do
  inistat <- getContents
  opts <- getArgs
  let (wait, iport) = parseOpts opts
  loop wait lc_counter4 $ toBits inistat

parseOpts :: [String] -> (Float, [Bin])
parseOpts [] = (wsec, defInput)
parseOpts (x:[]) = ((read :: String -> Float) x, defInput)
parseOpts (x:y:_) = ((read :: String -> Float) x, toBits y)

loop :: Float -> LogicCircuit -> [Bin] -> IO ()
loop w lc is = do
  let os = lc ([sHI, sHI] ++ is ++ toBits "0000")
  putStatus os
  threadDelay $ floor (w * 1000 * 1000)
  loop w lc os

putStatus :: [Bin] -> IO ()
putStatus os = putStrLn $ toStr os

