--
-- Flip Flop
--

module Logic.FlipFlop where

import Logic.BasicGate

{-
SR-Flip-Flop

-}

lc_srff :: LogicCircuit
lc_srff (s:r:q:q':_) = [s, r, x, x']
  where
    x_ = head $ lc_nand [s, q']
    x' = head $ lc_nand [r, x_]
    x  = head $ lc_nand [s, x']

