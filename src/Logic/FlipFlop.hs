--
-- Flip Flop
--

module Logic.FlipFlop where

import Logic.BasicGate

--
-- Flip Flop logics
--  * '!' means negative logic.

{-
SR-FlipFlop

  IN : [S,R,Q,!Q]
  OUT: [Q,!Q]
-}

lc_srff :: LogicCircuit
lc_srff (s:r:q:q':_) = [x, x']
  where
    x_ = head $ lc_nand [s, q']
    x' = head $ lc_nand [r, x_]
    x  = head $ lc_nand [s, x']

{-
D-FlipFlop (edge trigger)

  IN : [!CL,!PR,D]
  OUT: [Q,!Q]
-}

lc_dff :: LogicCircuit
lc_dff (False:_)      = [False, True]
lc_dff (True:False:_) = [True, False]
lc_dff (_:_:d:_)      = [d, not d]
