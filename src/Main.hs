
module Main where

import BasicGate

main :: IO ()
main = do
  let i1 = [True, True, True]
  putStrLn $ show $ lg_and i1
  let i2 = [True, False, True]
  putStrLn $ show $ lg_and i2
  let i3 = [True, False, False]
  putStrLn $ show $ lg_or i3
  let i4 = [False, False, False]
  putStrLn $ show $ lg_or i4
  let i5 = [False, True, False]
  putStrLn $ show $ lg_not i5

