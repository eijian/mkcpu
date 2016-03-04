--
-- Register
--

module Logic.Register where

import Logic.BasicGate
import Logic.FlipFlop
import Logic.ComplexLogic

{- |
1bit register

  IN : [CLR,LD,D0,D1]
  OUT: [Q]

>>> lc_register1 [sLO, sLO, sHI, sHI] == [sLO]  -- clear
True
>>> lc_register1 [sHI, sLO, sHI, sLO] == [sHI]  -- select D0
True
>>> lc_register1 [sHI, sLO, sLO, sHI] == [sLO]  -- select D0
True
>>> lc_register1 [sHI, sHI, sHI, sLO] == [sLO]  -- select D1
True
>>> lc_register1 [sHI, sHI, sLO, sHI] == [sHI]  -- select D1
True

-}

lc_register1 :: LogicCircuit
lc_register1 (c:l:d0:d1:_) = take 1 $ lc_dff_cp ([c, sHI] ++ d)
  where
    d = lc_multiplexer2ch [l, d0, d1]

{- |
4bit register

  IN : [CLR,LD,DA0,DB0,DC0,DD0,DA1,DB1,DC1,DD1]
  OUT: [Q0,Q1,Q2,Q3]

-}

lc_register4 :: LogicCircuit
lc_register4 (c:l:ds)
  | length ds < 4 * 2 = error "no enouth data"
  | otherwise = concat $ map (procReg1 c l) $ zip d0 d1
  where
    w = 4
    d0 = take w ds
    d1 = take w $ drop w ds

    procReg1 :: Bin -> Bin -> (Bin, Bin) -> [Bin]
    procReg1 c l (d0, d1) = lc_register1 [c, l, d0, d1]


