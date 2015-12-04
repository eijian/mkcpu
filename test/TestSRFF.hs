
module Main where

import BasicGate

main :: IO ()
main = do
  let i1 = [True, True, False, True]
      o1 = lg_srff i1
      i2 = reset_term [True, False, False, False] o1
      o2 = lg_srff i2
      i3 = set_term [True, False, False, False] o2
      o3 = lg_srff i3
      i4 = reset_term [False, True, False, False] o3
      o4 = lg_srff i4
      i5 = set_term [False, True, False, False] o4
      o5 = lg_srff i5
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





