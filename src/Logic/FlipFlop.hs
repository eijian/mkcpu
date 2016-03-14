--
-- Flip Flop
--

module Logic.FlipFlop (
  lc_srff
, lc_dff
, lc_dff_cp
) where

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
>>> lc_srff [sHI, sHI, sHI, sHI] == [sLO,sHI]
True
>>> lc_srff [sLO, sHI, sHI, sLO] == [sHI,sLO]
True
>>> lc_srff [sHI, sHI, sHI, sLO] == [sHI,sLO]
True
>>> lc_srff [sHI, sLO, sHI, sLO] == [sLO,sHI]
True
>>> lc_srff [sHI, sHI, sLO, sHI] == [sLO,sHI]
True
>>> lc_srff [sLO, sLO, sLO, sLO] == [sHI,sHI]
True
-}

lc_srff :: LogicCircuit
lc_srff (s:r:q0:q0':_) = [q, q']
  where
    q_ = s !&> q0'
    q' = r !&> q_
    q  = s !&> q'
lc_srff []       = lc_srff [sHI, sHI, sLO, sLO]
lc_srff (s:r:[]) = lc_srff [s, r, sLO, sLO]

{- |
D-FlipFlop (edge trigger)
  * only programing logic

  IN : [D]
  OUT: [Q,!Q]

>>> lc_dff' [sLO] == [sLO,sHI]
True
>>> lc_dff' [sHI] == [sHI,sLO]
True
-}

lc_dff' :: LogicCircuit
lc_dff' (d:_) = [d, not d]

{- |
D-FlipFlop (edge trigger)

  IN : [D]
  OUT: [Q,!Q]

>>> lc_dff [sLO] == lc_dff' [sLO]
True
>>> lc_dff [sHI] == lc_dff' [sHI]
True
-}

lc_dff :: LogicCircuit
lc_dff [] = lc_dff [sLO]
lc_dff (d:_) = lc_srff [x1', x2', sLO, sHI]
  where
    -- before clock (C = LO)
    x1 = sHI  -- because C = LO
    x2 = sHI  -- because C = LO
    x3 = d  !&> x2
    x0 = x1 !&> x3
    -- clock in (C = HI)
    x1' = sHI !&> x0
    x2' = (!>) (sHI &> x1' &> x3)  -- I can't use NAND op...

{-|
D-FlipFlop (edge trigger) with Clear/Preset

  IN : [!CL,!PR,D]
  OUT: [Q,!Q]

>>> lc_dff_cp [sLO, sLO, sLO] == [sLO,sHI]
True
>>> lc_dff_cp [sLO, sLO, sHI] == [sLO,sHI]
True
>>> lc_dff_cp [sLO, sHI, sLO] == [sLO,sHI]
True
>>> lc_dff_cp [sLO, sHI, sHI] == [sLO,sHI]
True
>>> lc_dff_cp [sHI, sLO, sLO] == [sHI,sLO]
True
>>> lc_dff_cp [sHI, sLO, sHI] == [sHI,sLO]
True
>>> lc_dff_cp [sHI, sHI, sLO] == [sLO,sHI]
True
>>> lc_dff_cp [sHI, sHI, sHI] == [sHI,sLO]
True
-}

lc_dff_cp :: LogicCircuit
lc_dff_cp (c:p:d:_) = lc_dff [d']
  where
    d' = c &> ((!>) p |> d)

