
module Main where

import Control.Concurrent
import System.Environment

import Logic.BasicGate
import Logic.FlipFlop
import Logic.ComplexLogic
import Logic.Register
import Logic.Rom
import Cpu

defClock :: Float
defClock = 1                -- default clock time = 1 sec

defInput :: [Bin]
defInput  = toBits "0000"   -- default value of Input port

main :: IO ()
main = do
  pg <- getContents
  opts <- getArgs
  let (clock, iport) = parseOpts opts
  putStrLn ("clock " ++ (show clock) ++ " sec; I/P " ++ toStr iport)
  -- CLR(1),CF(1),A(4),B(4),PC(4)
  let stat = take 14 $ repeat sLO
  loop 0 clock lc_td4_dummy stat iport (createRom pg)

-- parse options
parseOpts :: [String] -> (Float, [Bin])
parseOpts [] = (defClock, defInput)
parseOpts (x:[]) = ((read :: String -> Float) x, defInput)
parseOpts (x:y:_) = ((read :: String -> Float) x, toBits y)

-- create ROM data
createRom :: String -> [Bin]
createRom rs = concat $ map reverse $ split8 rs'
  where
    rs' = take 128 (toBits rs ++ repeat sLO)   -- 128 bits = 16 bytes
    
{-
loop : loop a logic with input and program

  IN : clock, circuit, stat, I/P, program
  OUT: stat
-}

loop :: Int -> Float -> LogicCircuit -> [Bin] -> [Bin] -> [Bin] -> IO ()
loop s w lc st ip pg = do
  let os = lc (st ++ ip ++ pg)
  putStatus s os
  threadDelay $ floor (w * 1000 * 1000)
  -- set CLR to HI and take a, b and pc from output
  let st' = [sHI] ++ (take 13 os)
  loop (s+1) w lc st' ip pg

{-
putStatus: put status of logic
  IN : [CF   carry flag (1),
        A    A register (4),
        B    B register (4),
        PC   program counter (4),
        OP   output port (4)]
-}

putStatus :: Int -> [Bin] -> IO ()
putStatus s os = do
  let st = "[CF:" ++ (toStr $ take 1 os) ++ "]"
        ++ "[A:" ++ (toStr $ reverse $ take 4 (drop 1 os)) ++ "]"
        ++ "[B:" ++ (toStr $ reverse $ take 4 (drop 5 os)) ++ "]"
        ++ "[PC:" ++ (toStr $ reverse $ take 4 (drop 9 os)) ++ "]"
        ++ "[OP:" ++ (toStr $ reverse $ take 4 (drop 13 os)) ++ "]"
  putStrLn ("step " ++ (show s) ++ "; " ++ st)

