
module Main where

import Logic.BasicGate

main :: IO ()
main = do
  let i1 = [True, True, True]
  putStrLn $ show $ lc_and i1
  let i2 = [True, False, True]
  putStrLn $ show $ lc_and i2
  let i3 = [True, False, False]
  putStrLn $ show $ lc_or i3
  let i4 = [False, False, False]
  putStrLn $ show $ lc_or i4
  let i5 = [False, True, False]
  putStrLn $ show $ lc_not i5

