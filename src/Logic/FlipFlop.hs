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

{- |
D-FlipFlop (edge trigger)

  IN : [!CL,!PR,D]
  OUT: [Q,!Q]

>>> lc_dff [sLO, sHI, sHI] == [sLO,sHI]
True
>>> lc_dff [sLO, sLO, sHI] == [sLO,sHI]
True
>>> lc_dff [sHI, sLO, sLO] == [sHI,sLO]
True
>>> lc_dff [sHI, sLO, sHI] == [sHI,sLO]
True
>>> lc_dff [sHI, sHI, sHI] == [sHI,sLO]
True
>>> lc_dff [sHI, sHI, sLO] == [sLO,sHI]
True
-}

lc_dff :: LogicCircuit
lc_dff (False:_)         = [sLO, sHI]
lc_dff (True :False:_)   = [sHI, sLO]
lc_dff (_    :_    :d:_) = [d, not d]
