--
-- Register
--

module Logic.Register where

import Logic.BasicGate
import Logic.FlipFlop
import Logic.ComplexLogic

{- |
1bit register

  IN : [!CLR,!LD,A,B]
  OUT: [Q]

  when CLO = LO -> Q = LO
  when LD  = HI -> Q = A
  when LD  = LO -> Q = B

>>> lc_register1 [sLO, sLO, sHI, sHI] == [sLO]  -- clear
True
>>> lc_register1 [sHI, sHI, sHI, sLO] == [sHI]  -- select A
True
>>> lc_register1 [sHI, sHI, sLO, sHI] == [sLO]  -- select A
True
>>> lc_register1 [sHI, sLO, sHI, sLO] == [sLO]  -- select B
True
>>> lc_register1 [sHI, sLO, sLO, sHI] == [sHI]  -- select B
True

-}

lc_register1 :: LogicCircuit
lc_register1 (c:l:a:b:_) = take 1 $ lc_dff_cp ([c, sHI] ++ d)
  where
    d = lc_multiplexer2ch [l, b, a]

{- |
4bit register

  IN : [!CLR,!LD,A0,A1,A2,A3,B0,B1,B2,B3]
  OUT: [Q0,Q1,Q2,Q3]

>>> let d0 = toBits "0000"
>>> let d1 = toBits "1111"
>>> let d2 = toBits "0101"
>>> let d3 = toBits "0011"
>>> lc_register4 ([sLO, sLO] ++ d1 ++ d2) == d0  -- when CLR = ON
True
>>> lc_register4 ([sLO, sHI] ++ d1 ++ d2) == d0  -- when CLR = ON
True
>>> lc_register4 ([sHI, sHI] ++ d1 ++ d2) == d1  -- when LD = OFF
True
>>> lc_register4 ([sHI, sLO] ++ d1 ++ d2) == d2  -- when LD = ON
True

-}

lc_register4 :: LogicCircuit
lc_register4 = lc_register 4

{- |
n bit register

  IN : bitwidth [!CLR,!LD,A(n bit),B(n bit)]
  OUT: [Q(n bit)]

  * 'bitwidth' has to be equal or bigger than 2

-- >>> lc_register 1 [sHI, sHI, sHI, sHI, sHI, sHI] == [sHI, sHI]
>>> lc_register 0 [sLO, sLO] == []
True
>>> lc_register 0 [sLO, sHI] == []
True
>>> lc_register 0 [sHI, sHI] == []
True
>>> lc_register 1 [sLO, sLO, sLO, sHI] == [sLO]  -- CLR
True
>>> lc_register 1 [sLO, sHI, sLO, sHI] == [sLO]  -- CLR
True
>>> lc_register 1 [sHI, sLO, sLO, sHI] == [sHI]  -- select B
True
>>> lc_register 1 [sHI, sLO, sHI, sLO] == [sLO]  -- select B
True
>>> lc_register 1 [sHI, sHI, sLO, sHI] == [sLO]  -- select A
True
>>> lc_register 1 [sHI, sHI, sHI, sLO] == [sHI]  -- select A
True

-}

lc_register :: Int -> LogicCircuit
lc_register w (c:l:ds)
  | w < 0       = error ("bit width is invalid (" ++ show w ++ ")")
  | len < w * 2 = error ("no enough input (" ++ show len ++ ")")
  | otherwise   = concat $ map (procReg1 c l) $ zip a b
  where
    len = length ds
    a = take w ds
    b = take w $ drop w ds
    procReg1 :: Bin -> Bin -> (Bin, Bin) -> [Bin]
    procReg1 c l (a, b) = lc_register1 [c, l, a, b]


{- |
Program Counter (4 bit)

  IN : [!CLR,!LD,A0,A1,A2,A3,B0,B1,B2,B3]
  OUT: [Q0,Q1,Q2,Q3]

    An: current count value
    Bn: count value set by outer
    Qn: next count value

>>> let d0 = toBits "0000"
>>> let d1 = toBits "1000"
>>> let d2 = toBits "0100"
>>> let d3 = toBits "1100"
>>> let d4 = toBits "0010"
>>> let d5 = toBits "1010"
>>> let d6 = toBits "0110"
>>> let d7 = toBits "1110"
>>> let d8 = toBits "0001"
>>> let d9 = toBits "1001"
>>> let d10 = toBits "0101"
>>> let d11 = toBits "1101"
>>> let d12 = toBits "0011"
>>> let d13 = toBits "1011"
>>> let d14 = toBits "0111"
>>> let d15 = toBits "1111"
>>> lc_counter4 ([sHI, sHI] ++ d0 ++ d15) == d1
True
>>> lc_counter4 ([sHI, sHI] ++ d1 ++ d15) == d2
True
>>> lc_counter4 ([sHI, sHI] ++ d2 ++ d15) == d3
True
>>> lc_counter4 ([sHI, sHI] ++ d3 ++ d15) == d4
True
>>> lc_counter4 ([sHI, sHI] ++ d4 ++ d15) == d5
True
>>> lc_counter4 ([sHI, sHI] ++ d5 ++ d15) == d6
True
>>> lc_counter4 ([sHI, sHI] ++ d6 ++ d15) == d7
True
>>> lc_counter4 ([sHI, sHI] ++ d7 ++ d15) == d8
True
>>> lc_counter4 ([sHI, sHI] ++ d8 ++ d15) == d9
True
>>> lc_counter4 ([sHI, sHI] ++ d9 ++ d15) == d10
True
>>> lc_counter4 ([sHI, sHI] ++ d10 ++ d15) == d11
True
>>> lc_counter4 ([sHI, sHI] ++ d11 ++ d15) == d12
True
>>> lc_counter4 ([sHI, sHI] ++ d12 ++ d15) == d13
True
>>> lc_counter4 ([sHI, sHI] ++ d13 ++ d15) == d14
True
>>> lc_counter4 ([sHI, sHI] ++ d14 ++ d15) == d15
True
>>> lc_counter4 ([sHI, sHI] ++ d15 ++ d15) == d0
True
>>> lc_counter4 ([sLO, sLO] ++ d5 ++ d15) == d0
True
>>> lc_counter4 ([sLO, sHI] ++ d5 ++ d15) == d0
True
>>> lc_counter4 ([sHI, sLO] ++ d5 ++ d15) == d15
True

-}

lc_counter4 :: LogicCircuit
lc_counter4 (c:l:ds) = lc_register4 ([c, l] ++ concat [d0, d1, d2, d3, b])
  where
    a = take 4 ds
    b = take 4 $ drop 4 ds
    a0 = a!!0
    a1 = a!!1
    a2 = a!!2
    a3 = a!!3
    d0 = lc_not [a0]
    d1 = lc_xor [a1, a0]
    d2 = lc_xor ([a2] ++ lc_and [a1, a0])
    d3 = lc_xor ([a3] ++ lc_and [a2, a1, a0])


