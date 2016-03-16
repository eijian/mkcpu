--
-- Complex Logic
--

module Logic.ComplexLogic (
  lc_decorder2
, lc_decorder4
, lc_multiplexer2ch
, lc_multiplexer4ch
) where

import Logic.BasicGate
import Logic.FlipFlop

-- DECORDERS -----------------

{- |
2bit decorder

  IN : [A,B]
  OUT: [!Y0,!Y1,!Y2,!Y3]

>>> lc_decorder2 [sLO, sLO] == lc_decorder2' [sLO, sLO]
True
>>> lc_decorder2 [sHI, sLO]  == lc_decorder2' [sHI, sLO]
True
>>> lc_decorder2 [sLO, sHI]  == lc_decorder2' [sLO, sHI]
True
>>> lc_decorder2 [sHI, sHI]   == lc_decorder2' [sHI, sHI]
True

>>> lc_decorder2 [sLO, sLO] == lc_decorder2'' [sLO, sLO]
True
>>> lc_decorder2 [sHI, sLO]  == lc_decorder2'' [sHI, sLO]
True
>>> lc_decorder2 [sLO, sHI]  == lc_decorder2'' [sLO, sHI]
True
>>> lc_decorder2 [sHI, sHI]   == lc_decorder2'' [sHI, sHI]
True

-}

lc_decorder2 :: LogicCircuit
lc_decorder2 (a:b:_) = [y0, y1, y2, y3]
  where
    a' = (!>) a
    b' = (!>) b
    y0 = a' !&> b'
    y1 = a  !&> b'
    y2 = a' !&> b
    y3 = a  !&> b

lc_decorder2' :: LogicCircuit
lc_decorder2' = decorder' 2

lc_decorder2'' :: LogicCircuit
lc_decorder2'' (False:False:_) = [sLO, sHI, sHI, sHI]
lc_decorder2'' (True:False:_)  = [sHI, sLO, sHI, sHI]
lc_decorder2'' (False:True:_)  = [sHI, sHI, sLO, sHI]
lc_decorder2'' (True:True:_)   = [sHI, sHI, sHI, sLO]

{- |
4bit decorder

  IN : [A,B,C,D]
  OUT: [!Y0,!Y1,!Y2,!Y3,!Y4,!Y5,!Y6,!Y7,!Y8,!Y9,!Y10,!Y11,!Y12,!Y13,!Y14,!Y15]

>>> let d0 = toBits "0000"
>>> lc_decorder4 d0 == lc_decorder4' d0
True
>>> let d1 = toBits "1000"
>>> lc_decorder4 d1 == lc_decorder4' d1
True
>>> let d2 = toBits "1111"
>>> lc_decorder4 d2 == lc_decorder4' d2
True

-}

lc_decorder4 :: LogicCircuit
lc_decorder4 (a:b:c:d:_) = [y0, y1, y2 , y3 , y4 , y5 , y6 , y7
                           ,y8, y9, y10, y11, y12, y13, y14, y15
                           ]
  where
    a' = (!>) a
    b' = (!>) b
    c' = (!>) c
    d' = (!>) d
    a'_b' = a' &> b'
    a_b'  = a  &> b'
    a'_b  = a' &> b
    a_b   = a  &> b
    c'_d' = c' &> d'
    c_d'  = c  &> d'
    c'_d  = c' &> d
    c_d   = c  &> d
    y0  = a'_b' !&> c'_d'
    y1  = a_b'  !&> c'_d'
    y2  = a'_b  !&> c'_d'
    y3  = a_b   !&> c'_d'
    y4  = a'_b' !&> c_d'
    y5  = a_b'  !&> c_d'
    y6  = a'_b  !&> c_d'
    y7  = a_b   !&> c_d'
    y8  = a'_b' !&> c'_d
    y9  = a_b'  !&> c'_d
    y10 = a'_b  !&> c'_d
    y11 = a_b   !&> c'_d
    y12 = a'_b' !&> c_d
    y13 = a_b'  !&> c_d
    y14 = a'_b  !&> c_d
    y15 = a_b   !&> c_d
{-
    [y0]  = lc_nand [a', b', c', d']
    [y1]  = lc_nand [a , b', c', d']
    [y2]  = lc_nand [a', b , c', d']
    [y3]  = lc_nand [a , b , c', d']
    [y4]  = lc_nand [a', b', c , d']
    [y5]  = lc_nand [a , b', c , d']
    [y6]  = lc_nand [a', b , c , d']
    [y7]  = lc_nand [a , b , c , d']
    [y8]  = lc_nand [a', b', c', d ]
    [y9]  = lc_nand [a , b', c', d ]
    [y10] = lc_nand [a', b , c', d ]
    [y11] = lc_nand [a , b , c', d ]
    [y12] = lc_nand [a', b', c , d ]
    [y13] = lc_nand [a , b', c , d ]
    [y14] = lc_nand [a', b , c , d ]
    [y15] = lc_nand [a , b , c , d ]
-}

lc_decorder4' :: LogicCircuit
lc_decorder4' = decorder' 4

decorder' :: Int -> LogicCircuit
decorder' b xs = map (\x -> if x == n then sLO else sHI) [0..mx]
  where
    mx = 2^b - 1
    n = bin2int (take b xs)

-- MULTIPLEXERS -----------------

{- |
2ch multiplexers

  IN : [a,y0,y1]
  OUT: [y?]

>>> lc_multiplexer2ch [sLO, sHI, sLO] == [sHI]
True
>>> lc_multiplexer2ch [sHI, sHI, sLO] == [sLO]
True
>>> lc_multiplexer2ch [sLO, sHI, sLO] == lc_multiplexer2ch' [sLO, sHI, sLO]
True
>>> lc_multiplexer2ch [sHI, sHI, sLO] == lc_multiplexer2ch' [sHI, sHI, sLO]
True
>>> lc_multiplexer2ch [sLO, sLO, sHI] == lc_multiplexer2ch' [sLO, sLO, sHI]
True
>>> lc_multiplexer2ch' [sHI, sLO, sHI] == lc_multiplexer2ch' [sHI, sLO, sHI]
True
-}

lc_multiplexer2ch :: LogicCircuit
lc_multiplexer2ch (a:y0:y1:_) = [a' &> y0 |> a &> y1]
  where
    a' = (!>) a

lc_multiplexer2ch' :: LogicCircuit
lc_multiplexer2ch' (False:y0:y1:_) = [y0]
lc_multiplexer2ch' (True :y0:y1:_) = [y1]

lc_multiplexer2ch'' :: LogicCircuit
lc_multiplexer2ch'' = multiplexer' 2

{- |
4ch multiplexer

  IN : [A,B,C0,C1,C2,C3]
  OUT: [Y]

>>> lc_multiplexer4ch [sLO, sLO, sHI, sLO, sLO, sLO] == [sHI]
True
>>> lc_multiplexer4ch [sLO, sLO, sLO, sLO, sLO, sLO] == [sLO]
True
>>> let d0 = toBits "001000"
>>> lc_multiplexer4ch d0 == lc_multiplexer4ch'' d0
True
>>> let d1 = toBits "000000"
>>> lc_multiplexer4ch d1 == lc_multiplexer4ch'' d1
True

-}

lc_multiplexer4ch :: LogicCircuit
lc_multiplexer4ch (a:b:c0:c1:c2:c3:_) = [y0 |> y1 |> y2 |> y3]
  where
    a' = (!>) a
    b' = (!>) b
    y0 = c0 &> a' &> b'
    y1 = c1 &> a  &> b'
    y2 = c2 &> a' &> b
    y3 = c3 &> a  &> b

lc_multiplexer4ch'' :: LogicCircuit
lc_multiplexer4ch'' = multiplexer' 4

{- |
general multiplexer function

-}

multiplexer' :: Int -> LogicCircuit
multiplexer' c xs = [xs'!!n]
  where
    b = floor (logBase 2 (fromIntegral c))
    n = bin2int $ take b xs
    xs' = drop b xs


-- ADDER ------------------------------

{- |
  1 bit full adder

  IN : [Ci,A,B]
  OUT: [C,S]

    Ci  : carry in
    C   : carry out
    A,B : value
    S   : answer

>>> lc_adder [sLO, sLO, sLO] == [sLO, sLO]
True
>>> lc_adder [sLO, sLO, sHI] == [sLO, sHI]
True
>>> lc_adder [sLO, sHI, sLO] == [sLO, sHI]
True
>>> lc_adder [sLO, sHI, sHI] == [sHI, sLO]
True
>>> lc_adder [sHI, sLO, sLO] == [sLO, sHI]
True
>>> lc_adder [sHI, sLO, sHI] == [sHI, sLO]
True
>>> lc_adder [sHI, sHI, sLO] == [sHI, sLO]
True
>>> lc_adder [sHI, sHI, sHI] == [sHI, sHI]
True

-}

lc_adder :: LogicCircuit
lc_adder (ci:a:b:_) = [c, s]
  where
    a_xor_b = a <+> b
    c = (a &> b) |> (ci &> a_xor_b)
    s = ci <+> a_xor_b

-- support functions

{- |
>>> bin2int [sLO, sLO]
0
>>> bin2int [sHI, sLO]
1
>>> bin2int [sLO, sHI]
2
>>> bin2int [sHI, sHI]
3
>>> bin2int [sLO, sHI, sLO, sHI]
10
>>> bin2int [sHI, sLO, sLO, sHI, sHI]
25
-}

bin2int :: [Bin] -> Int
bin2int [] = 0
bin2int (x:xs) = (bin2i x) + 2 * bin2int xs
