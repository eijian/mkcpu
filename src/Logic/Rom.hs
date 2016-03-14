--
-- ROM module
--

module Logic.Rom (
  lc_rom16
) where

import Logic.BasicGate
import Logic.ComplexLogic

msize = 16 :: Int

{- |
  ROM module (16 bytes)

  IN : [A0,A1,A2,A3,D00,D01,D02,...D07,D10,D11,...D17,...Df7]
  OUT: [Y0,Y1,...Y7]

>>> let m00 = toBits "00000000"
>>> let m01 = toBits "00000001"
>>> let m02 = toBits "00000010"
>>> let m03 = toBits "00000011"
>>> let m04 = toBits "00000100"
>>> let m05 = toBits "00000101"
>>> let mem = m00 ++ m01 ++ m02 ++ m03 ++ m04 ++ m05
>>> length mem
48
>>> lc_rom16 ([sLO, sLO, sLO, sLO] ++ mem) == m00
True
>>> lc_rom16 ([sHI, sLO, sLO, sLO] ++ mem) == m01
True
>>> lc_rom16 ([sLO, sHI, sLO, sLO] ++ mem) == m02
True
>>> lc_rom16 ([sHI, sHI, sLO, sLO] ++ mem) == m03
True
>>> lc_rom16 ([sLO, sLO, sHI, sLO] ++ mem) == m04
True
>>> lc_rom16 ([sHI, sLO, sHI, sLO] ++ mem) == m05
True
>>> lc_rom16 ([sLO, sHI, sHI, sLO] ++ mem) == m00
True
>>> lc_rom16 ([sHI, sHI, sHI, sHI] ++ mem) == m00
True

-}

lc_rom16 :: LogicCircuit
lc_rom16 xs = lc_not $ concat $ map (\x -> mergeBits x omem) [0..7]
  where
    mem = split8 $ take (8*msize) ((drop 4 xs) ++ repeat sLO)
    adr = lc_decorder4 $ take 4 xs
    omem = map toSwitch (zip adr mem) -- out of switches (16 bytes)

{- |
  toSwitch

  IN : tuple of Bin and [Bin]
  OUT: list of Bin

>>> let m = toBits "11001010"
>>> let h = toBits "11111111"
>>> toStr $ toSwitch (sHI, m)
"11111111"
>>> toStr $ toSwitch (sLO, m)
"00110101"

-}

toSwitch :: (Bin, [Bin]) -> [Bin]
toSwitch (a, ms) = lc_dipswitch (a:ms)

{- |
  mergeBits

>>> let m1 = toBits "1010"
>>> let m2 = toBits "1100"
>>> mergeBits 0 [m1, m2] == [sHI]
True
>>> mergeBits 1 [m1, m2] == [sLO]
True
>>> mergeBits 2 [m1, m2] == [sLO]
True
>>> mergeBits 3 [m1, m2] == [sLO]
True

-}

mergeBits :: Int -> [[Bin]] -> [Bin]
mergeBits n ms = lc_and $ map (!!n) ms
    
{- |
  DIP Switch (8bit)

  IN : [A, S0, S1, ..., S7]
  OUT: [!S0, !S1, ..., !S7]    (A=sLO)
       [sHI, sHI, ..., sHI] (A=sHI)

>>> let i1 = toBits "11010101"
>>> let o1 = toBits "11111111"
>>> lc_dipswitch (sHI:i1) == o1
True
>>> lc_dipswitch (sLO:i1) == lc_not i1
True
>>> let i2 = toBits "1010"
>>> toStr $ lc_dipswitch (sLO:i2)
"01011111"
>>> toStr $ lc_dipswitch [sLO]
"11111111"
>>> toStr $ lc_dipswitch [sHI]
"11111111"

-}

lc_dipswitch :: LogicCircuit
lc_dipswitch (a:xs)
  | a == sHI = take 8 $ repeat sHI
  | a == sLO = take 8 ((lc_not xs) ++ repeat sHI)

{- |
  split list by 8bit

>>> let i1 = toBits "11001010"
>>> split8 i1 == [i1]
True
>>> split8 (i1 ++ i1) == [i1, i1]
True
>>> let i2 = toBits "110010"
>>> let i3 = toBits "11001011"
>>> split8 (i1 ++ i2) == [i1, i3]
True

-}

split8 :: [Bin] -> [[Bin]]
split8 = split 8

split :: Int -> [Bin] -> [[Bin]]
split w [] = []
split w xs
  |length xs < w = [take w (xs ++ repeat sLO)]
  |otherwise     = l:(split w ls)
    where
      l  = take w xs
      ls = drop w xs

{-
* バイト列から128bitのリスト作る
 
-}

