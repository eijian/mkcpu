
module Main where

import Logic.BasicGate

main :: IO ()
main = do
  let i1 = [sHI, sHI, sHI]
  putStrLn $ show $ lc_and i1
  let i2 = [sHI, sLO, sHI]
  putStrLn $ show $ lc_and i2
  let i3 = [sHI, sLO, sLO]
  putStrLn $ show $ lc_or i3
  let i4 = [sLO, sLO, sLO]
  putStrLn $ show $ lc_or i4
  let i5 = [sLO, sHI, sLO]
  putStrLn $ show $ lc_not i5

