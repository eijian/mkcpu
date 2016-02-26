--
-- Register
--

module Logic.Register where

import Logic.BasicGate
import Logic.FlipFlop
import Logic.ComplexLogic

{- |
1bit register

  IN : [CLR,LD,A,B]
  OUT: [Q]

>>> lc_register1 [sLO, sLO, sHI, sHI] == [sLO]  -- clear
True
>>> lc_register1 [sHI, sLO, sHI, sLO] == [sHI]  -- select A
True
>>> lc_register1 [sHI, sLO, sLO, sHI] == [sLO]  -- select A
True
>>> lc_register1 [sHI, sHI, sHI, sLO] == [sLO]  -- select B
True
>>> lc_register1 [sHI, sHI, sLO, sHI] == [sHI]  -- select B
True

-}

lc_register1 :: LogicCircuit
lc_register1 (c:l:a:b:_) = take 1 $ lc_dff_cp ([c, sHI] ++ d)
  where
    d = lc_multiplexer2ch [l, a, b]

{- |
4bit register

  IN : [CLR,LD,A0,A1,A2,A3,B0,B1,B2,B3]
  OUT: [Q0,Q1,Q2,Q3]

>>> let d0 = toBits "0000"
>>> let d1 = toBits "1111"
>>> let d2 = toBits "0101"
>>> let d3 = toBits "0011"
>>> lc_register4 ([sLO, sLO] ++ d1 ++ d2) == d0  -- when CLR = ON
True
>>> lc_register4 ([sLO, sHI] ++ d1 ++ d2) == d0  -- when CLR = ON
True
>>> lc_register4 ([sHI, sLO] ++ d1 ++ d2) == d1  -- when LD = OFF
True
>>> lc_register4 ([sHI, sHI] ++ d1 ++ d2) == d2  -- when LD = ON
True

-}

lc_register4 :: LogicCircuit
lc_register4 = lc_register 4

{- |
n bit register

  IN : bitwidth [CLR,LD,A(n bit),B(nbit)]
  OUT: [Q(n bit)]

  * 'bitwidth' has to be equal or bigger than 2

-- >>> lc_register 1 [sHI, sHI, sHI, sHI, sHI, sHI] == [sHI, sHI]

-}

lc_register :: Int -> LogicCircuit
lc_register w (c:l:ds)
  | w < 2       = error ("bit width is invalid (" ++ show w ++ ")")
  | len < w * 2 = error ("no enough input (" ++ show len ++ ")")
  | otherwise   = concat $ map (procReg1 c l) $ zip a b
  where
    len = length ds
    a = take w ds
    b = take w $ drop w ds
    procReg1 :: Bin -> Bin -> (Bin, Bin) -> [Bin]
    procReg1 c l (a, b) = lc_register1 [c, l, a, b]


