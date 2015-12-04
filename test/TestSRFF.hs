
module Main where

import Logic.BasicGate
import Logic.FlipFlop

main :: IO ()
main = do
  let i1 = [True, True] ++ [False, False]
      o1 = lc_srff i1
      i2 = [False, True] ++ o1
      o2 = lc_srff i2
      i3 = [True, True] ++ o2
      o3 = lc_srff i3
      i4 = [True, False] ++ o3
      o4 = lc_srff i4
      i5 = [True, True] ++ o4
      o5 = lc_srff i5
  putStrLn ("I1=" ++ show i1)
  putStrLn ("O1=" ++ show o1)
  putStrLn ("I2=" ++ show i2)
  putStrLn ("O2=" ++ show o2)
  putStrLn ("I3=" ++ show i3)
  putStrLn ("O3=" ++ show o3)
  putStrLn ("I4=" ++ show i4)
  putStrLn ("O4=" ++ show o4)
  putStrLn ("I5=" ++ show i5)
  putStrLn ("O5=" ++ show o5)





