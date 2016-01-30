--
-- Flip Flop
--

module Logic.FlipFlop where

import Logic.BasicGate

--
-- Flip Flop logics
--  * '!' means negative logic.

{- |
SR-FlipFlop

  IN : [S,R,Q,!Q]
  OUT: [Q,!Q]

>>> lc_srff [sHI, sHI, sLO, sLO] == [sHI,sLO]
True
>>> lc_srff [sLO, sHI, sHI, sLO] == [sHI,sLO]
True
>>> lc_srff [sHI, sHI, sHI, sLO] == [sHI,sLO]
True
>>> lc_srff [sHI, sLO, sHI, sLO] == [sLO,sHI]
True
>>> lc_srff [sHI, sHI, sLO, sHI] == [sLO,sHI]
True
-}

lc_srff :: LogicCircuit
lc_srff (s:r:q:q':_) = [x, x']
  where
    x_ = head $ lc_nand [s, q']
    x' = head $ lc_nand [r, x_]
    x  = head $ lc_nand [s, x']
lc_srff (s:r:_) = lc_srff [s, r, sLO, sLO]
lc_srff _ = lc_srff [sHI, sHI, sLO, sLO]

{- |
D-FlipFlop (edge trigger)

  IN : [!CL,!PR,D]
  OUT: [Q,!Q]

>>> lc_dff' [sLO, sHI, sHI] == [sLO,sHI]
True
>>> lc_dff' [sLO, sLO, sHI] == [sLO,sHI]
True
>>> lc_dff' [sHI, sLO, sLO] == [sHI,sLO]
True
>>> lc_dff' [sHI, sLO, sHI] == [sHI,sLO]
True
>>> lc_dff' [sHI, sHI, sHI] == [sHI,sLO]
True
>>> lc_dff' [sHI, sHI, sLO] == [sLO,sHI]
True
-}

lc_dff' :: LogicCircuit
lc_dff' (False:_)         = [sLO, sHI]
lc_dff' (True :False:_)   = [sHI, sLO]
lc_dff' (_    :_    :d:_) = [d, not d]

{- |

>>> lc_dff [sHI, sHI, sHI] == [sHI,sLO]
True
>>> lc_dff [sHI, sHI, sLO] == [sLO,sHI]
True
>>> lc_dff [sLO, sHI, sHI] == [sLO,sHI]
True
>>> lc_dff [sLO, sLO, sHI] == [sLO,sHI]
True
>>> lc_dff [sHI, sLO, sLO] == [sHI,sLO]
True
>>> lc_dff [sHI, sLO, sHI] == [sHI,sLO]
True
-}

lc_dff :: LogicCircuit
lc_dff [] = lc_dff [sLO]
--lc_dff (False:_) = lc_srff [sLO, sHI, sLO]
--lc_dff (True:False:_) = lc_srff [sHI, sLO, sLO]
lc_dff (c:r:d:_) = lc_srff (x1' ++ x2' ++ q)
  where
    d' = lc_and ([c] ++ lc_or (lc_not [r] ++ [d]))
    -- before clock
    x1 = sHI  -- because C = LO
    x2 = sHI  -- because C = LO
    x3 = lc_nand (d' ++ [x2])
    x0 = lc_nand ([x1] ++ x3)
    q = lc_srff [x1, x2, sLO, sHI]  -- default Q = LO, Q'= HI
    -- clock in (C = HI)
    x1' = lc_nand ([sHI] ++ x0)
    x2' = lc_nand ([sHI] ++ x1' ++ x3)


