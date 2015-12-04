--
-- Flip Flop
--

module Logic.FlipFlop where

import Logic.BasicGate

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
  * '!' means negative logic.
-}

lc_dff :: LogicCircuit
lc_dff (True:_) = [False, True]
lc_dff (False:d:_) = [d, q']
  where
    q' = head $ lc_not [d]

