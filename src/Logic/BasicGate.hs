--
-- Basic Gate
--

module Logic.BasicGate (
  Bin
, LogicCircuit
, sHI
, sLO
, toBits
, toStr
, bin2i
, lc_and
, lc_or
, lc_not
, lc_nand
, lc_nor
, lc_xor
, (&>)
, (|>)
, (!>)
, (!&>)
, (!|>)
, (<+>)
) where

import Data.List

--
-- Basic definitions
--

type Bin = Bool
type LogicCircuit = [Bin] -> [Bin]

sHI = True  :: Bin
sLO = False :: Bin

--
-- UTILITIES
--

c2bin :: Char -> Bin
c2bin '0' = sLO
c2bin '1' = sHI

bin2c :: Bin -> Char
bin2c b
  |b == sLO = '0'
  |b == sHI = '1'

i2bin :: Int -> Bin
i2bin 0 = sLO
i2bin 1 = sHI

bin2i :: Bin -> Int
bin2i b
  |b == sLO = 0
  |b == sHI = 1


{- |
  toBits: produce 'Bin' list from input String

  IN : "00101101101010110000101110..."
  OUT: [sLO, sLO, sHI, sLO, sHI, sHI, ...]

>>> toBits "00101101" == [sLO, sLO, sHI, sLO, sHI, sHI, sLO, sHI]
True
>>> toBits "11001010" == [sHI, sHI, sLO, sLO, sHI, sLO, sHI, sLO]
True

-}

toBits :: String -> [Bin]
toBits cs = map (c2bin) $ filter (\c -> isInfixOf [c] "01") cs

{- |
  toStr: convert from [Bin] to string

-}

toStr :: [Bin] -> String
toStr xs = map (bin2c) xs

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
set_term fs [] = fs
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

--
-- LOGICAL GATES
--

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

{- |
XOR gate (two input)

>>> lc_xor [sLO, sLO] == [sLO]
True
>>> lc_xor [sLO, sHI] == [sHI]
True
>>> lc_xor [sHI, sLO] == [sHI]
True
>>> lc_xor [sHI, sHI] == [sLO]
True


-}

lc_xor :: LogicCircuit
lc_xor (a:b:_) = [(a &> b') |> (b &> a')]
  where
    a' = (!>) a
    b' = (!>) b


-- UTILITY OPERATORS

-- NOT
(!>) :: Bin -> Bin
(!>) a = head $ lc_not [a]

-- AND
(&>) :: Bin -> Bin -> Bin
a &> b = head $ lc_and [a, b]

-- OR
(|>) :: Bin -> Bin -> Bin
a |> b = head $ lc_or [a, b]

-- NAND
(!&>) :: Bin -> Bin -> Bin
a !&> b = head $ lc_nand [a, b]

-- NOR
(!|>) :: Bin -> Bin -> Bin
a !|> b = head $ lc_nor [a, b]

-- XOR
(<+>) :: Bin -> Bin -> Bin
a <+> b = head $ lc_xor [a, b]

infixr 8 !>
infixr 7 &>
infixr 7 !&>
infixr 6 |>
infixr 6 !|>
infixr 6 <+>

