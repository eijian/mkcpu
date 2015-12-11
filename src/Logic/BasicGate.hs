--
-- Basic Gate
--

module Logic.BasicGate where

type Bin = Bool
type LogicCircuit = [Bin] -> [Bin]

-- set/reset terminals

{- |
set terminals

>>> set_term [False, True] [True, False]
[True,True]
>>> set_term [True, True] [True, False]
[True,True]
>>> set_term [False, False] [True, False]
[True,False]
-}

set_term :: [Bin] -> LogicCircuit
set_term [] xs = xs
set_term fs xs = zipWith (||) xs fs

{- |
reset terminals

>>> reset_term [False, True] [True, True]
[True,False]
>>> reset_term [True, True] [True, False]
[False,False]
>>> reset_term [False, False] [True, False]
[True,False]
-}

reset_term :: [Bin] -> LogicCircuit
reset_term [] xs = xs
reset_term fs xs = zipWith (&&) xs (lc_not fs)

-- logical gates

{- |
AND gate (multiple input)

>>> lc_and [True, True]
[True]
>>> lc_and [True, False]
[False]
>>> lc_and [False, True]
[False]
>>> lc_and [False, False]
[False]
-}

lc_and :: LogicCircuit
lc_and [] = [False]
lc_and xs = [and xs]

{- |
OR gate (multiple input)

>>> lc_or [True, True]
[True]
>>> lc_or [True, False]
[True]
>>> lc_or [False, True]
[True]
>>> lc_or [False, False]
[False]
-}

lc_or :: LogicCircuit
lc_or [] = [False]
lc_or xs = [or xs]

{- |
NOT gate (multiple input)

>>> lc_not [True]
[False]
>>> lc_not [False]
[True]
>>> lc_not [False, True]
[True,False]
>>> lc_not [True, True]
[False,False]
-}

lc_not :: LogicCircuit
lc_not [] = [True]
lc_not xs = map (not) xs

{- |
NAND gate (multiple input)

>>> lc_nand [True, True]
[False]
>>> lc_nand [True, False]
[True]
>>> lc_nand [False, True]
[True]
>>> lc_nand [False, False]
[True]
-}

lc_nand :: LogicCircuit
lc_nand = lc_not . lc_and

{- |
NOR gate (multiple input)

>>> lc_nor [True, True]
[False]
>>> lc_nor [True, False]
[False]
>>> lc_nor [False, True]
[False]
>>> lc_nor [False, False]
[True]
-}

lc_nor :: LogicCircuit
lc_nor = lc_not . lc_or

