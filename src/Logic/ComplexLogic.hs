--
-- Complex Logic
--

module Logic.ComplexLogic where

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
    [a', b'] = lc_not [a, b]
    [y0] = lc_nand [a', b']
    [y1] = lc_nand [a, b']
    [y2] = lc_nand [a', b]
    [y3] = lc_nand [a, b]

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

>>> lc_decorder4 [sLO, sLO, sLO, sLO] == lc_decorder4' [sLO, sLO, sLO, sLO]
True
>>> lc_decorder4 [sHI, sLO, sLO, sLO] == lc_decorder4' [sHI, sLO, sLO, sLO]
True
>>> lc_decorder4 [sHI, sHI, sHI, sHI] == lc_decorder4' [sHI, sHI, sHI, sHI]
True

-}

lc_decorder4 :: LogicCircuit
lc_decorder4 (a:b:c:d:_) = [y0, y1, y2 , y3 , y4 , y5 , y6 , y7
                           ,y8, y9, y10, y11, y12, y13, y14, y15
                           ]
  where
    [a', b', c', d'] = lc_not [a, b, c, d]
    [a'_b'] = lc_and [a', b']
    [a_b' ] = lc_and [a , b']
    [a'_b ] = lc_and [a', b ]
    [a_b  ] = lc_and [a , b ]
    [c'_d'] = lc_and [c', d']
    [c_d' ] = lc_and [c , d']
    [c'_d ] = lc_and [c', d ]
    [c_d  ] = lc_and [c , d ]
    [y0]  = lc_nand [a'_b', c'_d']
    [y1]  = lc_nand [a_b' , c'_d']
    [y2]  = lc_nand [a'_b , c'_d']
    [y3]  = lc_nand [a_b  , c'_d']
    [y4]  = lc_nand [a'_b', c_d' ]
    [y5]  = lc_nand [a_b' , c_d' ]
    [y6]  = lc_nand [a'_b , c_d' ]
    [y7]  = lc_nand [a_b  , c_d' ]
    [y8]  = lc_nand [a'_b', c'_d]
    [y9]  = lc_nand [a_b' , c'_d]
    [y10] = lc_nand [a'_b , c'_d]
    [y11] = lc_nand [a_b  , c'_d]
    [y12] = lc_nand [a'_b', c_d]
    [y13] = lc_nand [a_b' , c_d]
    [y14] = lc_nand [a'_b , c_d]
    [y15] = lc_nand [a_b  , c_d]
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
>>> lc_multiplexer2ch' [sLO, sHI, sLO] == lc_multiplexer2ch'' [sLO, sHI, sLO]
True
>>> lc_multiplexer2ch' [sHI, sHI, sLO] == lc_multiplexer2ch'' [sHI, sHI, sLO]
True
-}

lc_multiplexer2ch :: LogicCircuit
lc_multiplexer2ch (a:y0:y1:_) = lc_or (lc_and [a', y0] ++ lc_and [a, y1])
  where
    [a'] = lc_not [a]

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
>>> lc_multiplexer4ch [sLO, sLO, sHI, sLO, sLO, sLO] == lc_multiplexer4ch'' [sLO, sLO, sHI, sLO, sLO, sLO]
True
>>> lc_multiplexer4ch [sLO, sLO, sLO, sLO, sLO, sLO] == lc_multiplexer4ch'' [sLO, sLO, sLO, sLO, sLO, sLO]
True

-}

lc_multiplexer4ch :: LogicCircuit
lc_multiplexer4ch (a:b:c0:c1:c2:c3:_) = lc_or (y0 ++ y1 ++ y2 ++ y3)
  where
    [a', b'] = lc_not [a, b]
    y0 = lc_and [c0, a', b']
    y1 = lc_and [c1, a, b']
    y2 = lc_and [c2, a', b]
    y3 = lc_and [c3, a, b]

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
bin2int (x:xs) = a + 2 * bin2int xs
  where
    a = if x == sHI then 1 else 0
