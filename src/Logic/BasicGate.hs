--
-- Basic Gate
--

module Logic.BasicGate where

type Bin = Bool
type LogicCircuit = [Bin] -> [Bin]

sHI :: Bin
sHI = True
sLO :: Bin
sLO = False

-- set/reset terminals

{- |
set terminals

>>> set_term [sLO, sHI] [sHI, sLO] == [sHI,sHI]
True
>>> set_term [sHI, sHI] [sHI, sLO] == [sHI,sHI]
True
>>> set_term [sLO, sLO] [sHI, sLO] == [sHI,sLO]
True
-}

set_term :: [Bin] -> LogicCircuit
set_term [] xs = xs
set_term fs xs = zipWith (||) xs fs

{- |
reset terminals

>>> reset_term [sLO, sHI] [sHI, sHI] == [sHI,sLO]
True
>>> reset_term [sHI, sHI] [sHI, sLO] == [sLO,sLO]
True
>>> reset_term [sLO, sLO] [sHI, sLO] == [sHI,sLO]
True
-}

reset_term :: [Bin] -> LogicCircuit
reset_term [] xs = xs
reset_term fs xs = zipWith (&&) xs (lc_not fs)

-- logical gates

{- |
AND gate (multiple input)

>>> lc_and [sHI, sHI] == [sHI]
True
>>> lc_and [sHI, sLO] == [sLO]
True
>>> lc_and [sLO, sHI] == [sLO]
True
>>> lc_and [sLO, sLO] == [sLO]
True
-}

lc_and :: LogicCircuit
lc_and [] = [sLO]
lc_and xs = [and xs]

{- |
OR gate (multiple input)

>>> lc_or [sHI, sHI] == [sHI]
True
>>> lc_or [sHI, sLO] == [sHI]
True
>>> lc_or [sLO, sHI] == [sHI]
True
>>> lc_or [sLO, sLO] == [sLO]
True
-}

lc_or :: LogicCircuit
lc_or [] = [sLO]
lc_or xs = [or xs]

{- |
NOT gate (multiple input)

>>> lc_not [sHI] == [sLO]
True
>>> lc_not [sLO] == [sHI]
True
>>> lc_not [sLO, sHI] == [sHI,sLO]
True
>>> lc_not [sHI, sHI] == [sLO,sLO]
True
-}

lc_not :: LogicCircuit
lc_not [] = [sHI]
lc_not xs = map (not) xs

{- |
NAND gate (multiple input)

>>> lc_nand [sHI, sHI] == [sLO]
True
>>> lc_nand [sHI, sLO] == [sHI]
True
>>> lc_nand [sLO, sHI] == [sHI]
True
>>> lc_nand [sLO, sLO] == [sHI]
True
-}

lc_nand :: LogicCircuit
lc_nand = lc_not . lc_and

{- |
NOR gate (multiple input)

>>> lc_nor [sHI, sHI] == [sLO]
True
>>> lc_nor [sHI, sLO] == [sLO]
True
>>> lc_nor [sLO, sHI] == [sLO]
True
>>> lc_nor [sLO, sLO] == [sHI]
True
-}

lc_nor :: LogicCircuit
lc_nor = lc_not . lc_or

