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

>>> lc_decorder2 [False, False] == lc_decorder2' [False, False]
True
>>> lc_decorder2 [True, False]  == lc_decorder2' [True, False]
True
>>> lc_decorder2 [False, True]  == lc_decorder2' [False, True]
True
>>> lc_decorder2 [True, True]   == lc_decorder2' [True, True]
True

>>> lc_decorder2 [False, False] == lc_decorder2'' [False, False]
True
>>> lc_decorder2 [True, False]  == lc_decorder2'' [True, False]
True
>>> lc_decorder2 [False, True]  == lc_decorder2'' [False, True]
True
>>> lc_decorder2 [True, True]   == lc_decorder2'' [True, True]
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
lc_decorder2' xs = decorder' 2 xs

lc_decorder2'' :: LogicCircuit
lc_decorder2'' (False:False:_) = [False, True, True, True]
lc_decorder2'' (True:False:_)  = [True, False, True, True]
lc_decorder2'' (False:True:_)  = [True, True, False, True]
lc_decorder2'' (True:True:_)   = [True, True, True, False]

{- |
4bit decorder

  IN : [A,B,C,D]
  OUT: [!Y0,!Y1,!Y2,!Y3,!Y4,!Y5,!Y6,!Y7,!Y8,!Y9,!Y10,!Y11,!Y12,!Y13,!Y14,!Y15]

>>> lc_decorder4 [False, False, False, False] == lc_decorder4' [False, False, False, False]
True
>>> lc_decorder4 [True, False, False, False] == lc_decorder4' [True, False, False, False]
True
>>> lc_decorder4 [True, True, True, True] == lc_decorder4' [True, True, True, True]
True

-}

lc_decorder4 :: LogicCircuit
lc_decorder4 (a:b:c:d:_) = [y0, y1, y2 , y3 , y4 , y5 , y6 , y7
                           ,y8, y9, y10, y11, y12, y13, y14, y15
                           ]
  where
    [a', b', c', d'] = lc_not [a, b, c, d]
    [a'_b'] = lc_and [a', b']
    [a_b']  = lc_and [a , b']
    [a'_b]  = lc_and [a', b ]
    [a_b]   = lc_and [a , b ]
    [c'_d'] = lc_and [c', d']
    [c_d']  = lc_and [c , d']
    [c'_d]  = lc_and [c', d ]
    [c_d]   = lc_and [c , d ]
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

lc_decorder4' :: LogicCircuit
lc_decorder4' xs = decorder' 4 xs

decorder' :: Int -> LogicCircuit
decorder' b xs = map (\x -> if x == n then False else True) [0..mx]
  where
    mx = 2^b - 1
    n = bin2int xs

-- SELECTORS -----------------

{- |
2ch selectors

  IN : [a,y0,y1]
  OUT: [y?]

>>> lc_selector2ch' [False, True, False]
[True]
>>> lc_selector2ch' [True, True, False]
[False]
>>> lc_selector2ch' [False, True, False] == lc_selector2ch'' [False, True, False]
True
>>> lc_selector2ch' [True, True, False] == lc_selector2ch'' [True, True, False]
True

-}

--lc_selector2ch :: LogicCircuit
--lc_selector2ch (a:xs) = not implemented

lc_selector2ch' :: LogicCircuit
lc_selector2ch' (False:y0:y1:_) = [y0]
lc_selector2ch' (True :y0:y1:_) = [y1]

lc_selector2ch'' :: LogicCircuit
lc_selector2ch'' xs = selector' 2 xs

{- |
general selector function

-}

selector' :: Int -> LogicCircuit
selector' c xs = [xs'!!n]
  where
    b = floor (logBase 2 (fromIntegral c))
    n = bin2int $ take b xs
    xs' = drop b xs


-- support functions

{- |
>>> bin2int [False, False]
0
>>> bin2int [True, False]
1
>>> bin2int [False, True]
2
>>> bin2int [True, True]
3
>>> bin2int [False, True, False, True]
10
>>> bin2int [True, False, False, True, True]
25
-}

bin2int :: [Bool] -> Int
bin2int [] = 0
bin2int (x:xs) = a + 2 * bin2int xs
  where
    a = if x == True then 1 else 0

