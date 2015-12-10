
module Main where

import Logic.BasicGate
import Logic.FlipFlop

main :: IO ()
main = do
  let i1 = [False, True, True]
      o1 = lc_dff i1
      i2 = [False, False, True]
      o2 = lc_dff i2
      i3 = [True, False, False]
      o3 = lc_dff i3
      i4 = [True, False, True]
      o4 = lc_dff i4
      i5 = [True, True, True]
      o5 = lc_dff i5
      i6 = [True, True, False]
      o6 = lc_dff i6
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
  putStrLn ("I6=" ++ show i6)
  putStrLn ("O6=" ++ show o6)





