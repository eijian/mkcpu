--
-- TD4 CPU
--

module Cpu (
  lc_td4
, lc_td4_st0
, lc_td4_st1
, lc_td4_st2
, lc_inst_decorder
) where

import Logic.BasicGate
import Logic.ComplexLogic
import Logic.FlipFlop
import Logic.Register
import Logic.Rom

{- |
TD4 CPU

  IN : [!C   clear (1),
        CF   carry flag (1),
        A    A register (4),
        B    B register (4),
        PC   program counter (4),
        IP   input port (4),
        ROM  ROM data 16 bytes (128)]
  OUT: [CF   carry flag (1),
        A    A register (4),
        B    B register (4),
        PC   program counter (4),
        OP   output port (4)]

-}

lc_td4 :: LogicCircuit
lc_td4 (c:cf:xs) = pc 
  where
    reg_a = take 4 xs
    reg_b = take 4 (drop 4 xs)
    pc    = take 4 (drop 8 xs)
    ip    = take 4 (drop 12 xs)
    rom   = drop 12 xs

{- |
instruction decorder

  IN : [OP0,OP1,OP2,OP3,CF]
  OUT: [SA,SB,LD0,LD1,LD2,LD3]

    OPn: operation code (upper 4 bit)
    CF : carry flag
    SA : Select A
    SB : Select B
    LDn: LOAD0 - LOAD3

>>> toStr $ lc_inst_decorder $ toBits "00000"  -- ADD A,Im
"000111"
>>> toStr $ lc_inst_decorder $ toBits "10000"  -- MOV A,B
"100111"
>>> toStr $ lc_inst_decorder $ toBits "01000"  -- IN A
"010111"
>>> toStr $ lc_inst_decorder $ toBits "11000"  -- MOV A,Im
"110111"
>>> toStr $ lc_inst_decorder $ toBits "00100"  -- MOV B,A
"001011"
>>> toStr $ lc_inst_decorder $ toBits "10100"  -- ADD B,Im
"101011"
>>> toStr $ lc_inst_decorder $ toBits "01100"  -- IN B
"011011"
>>> toStr $ lc_inst_decorder $ toBits "11100"  -- MOV B,Im
"111011"
>>> toStr $ lc_inst_decorder $ toBits "10010"  -- OUT B
"101101"
>>> toStr $ lc_inst_decorder $ toBits "11010"  -- OUT Im
"111101"
>>> toStr $ lc_inst_decorder $ toBits "01110"  -- JNC(C=0) Im
"111110"
>>> toStr $ lc_inst_decorder $ toBits "01111"  -- JNC(C=1) Im
"111111"
>>> toStr $ lc_inst_decorder $ toBits "11110"  -- JMP Im
"111110"

-}

lc_inst_decorder :: LogicCircuit
lc_inst_decorder (op0:op1:op2:op3:c:_) = [sa, sb, l0, l1, l2, l3]
  where
    sa = op0 |> op3
    sb = op1
    nop0 = (!>) op0
    nop2 = (!>) op2
    nop3 = (!>) op3
    l0 = op2  |> op3
    l1 = nop2 |> op3
    l2 = op2  |> nop3
    l3 = nop2 |> nop3 |> (nop0 &> c)

{-
lc_td4_st0: step 0 (through all inputs to outputs)
  IN : [!C   clear (1),
        CF   carry flag (1),
        A    A register (4),
        B    B register (4),
        PC   program counter (4),
        OP   output port (4),
        IP   input port (4),
        ROM  ROM data 16 bytes (128)]
  OUT: [CF   carry flag (1),
        A    A register (4),
        B    B register (4),
        PC   program counter (4),
        OP   output port (4)]

-}

lc_td4_st0 :: LogicCircuit
lc_td4_st0 xs = concat [cf, a, b, pc, op]
  where
    cl  = take 1 xs
    cf  = take 1 (drop 1 xs)
    a   = take 4 (drop 2 xs)
    b   = take 4 (drop 6 xs)
    pc  = take 4 (drop 10 xs)
    op  = take 4 (drop 14 xs)
    ip  = take 4 (drop 18 rom)
    rom = drop 22 xs

{-
lc_td4_st1: step 1 (output via FlipFlop)

-}

lc_td4_st1 :: LogicCircuit
lc_td4_st1 xs = concat [cf', a', b', pc', op']
  where
    cl  = take 1 xs
    cf  = take 1 (drop 1 xs)
    a   = take 4 (drop 2 xs)
    b   = take 4 (drop 6 xs)
    pc  = take 4 (drop 10 xs)
    op  = take 4 (drop 14 xs)
    ip  = take 4 (drop 18 rom)
    rom = drop 22 xs
    v0  = toBits "0000"
    cf' = take 1 $ lc_dff_cp (cl ++ [sHI] ++ cf)
    a'  = lc_register4 (cl ++ [sHI] ++ a  ++ v0)
    b'  = lc_register4 (cl ++ [sHI] ++ b  ++ v0)
    pc' = lc_counter4  (cl ++ [sHI] ++ pc ++ v0)
    op' = lc_register4 (cl ++ [sHI] ++ op ++ v0)

{-
lc_td4_st2: step 2 (add immediate value to a)
-}

lc_td4_st2 :: LogicCircuit
lc_td4_st2 xs = concat [cf', a', b', pc', op']
  where
    cl  = take 1 xs
    cf  = take 1 (drop 1 xs)
    a   = take 4 (drop 2 xs)
    b   = take 4 (drop 6 xs)
    pc  = take 4 (drop 10 xs)
    op  = take 4 (drop 14 xs)
    ip  = take 4 (drop 18 xs)
    rom = drop 22 xs
    rdata = lc_rom16 (pc ++ rom) -- get data addressed by PC
    v0  = toBits "0000"
    im  = take 4 rdata
    ss  = lc_adder (a ++ im)
    s0  = take 4 ss
    c0  = drop 4 ss
    cf' = take 1 $ lc_dff_cp (cl ++ [sHI] ++ c0)
    a'  = lc_register4 (cl ++ [sLO] ++ a  ++ s0)
    b'  = lc_register4 (cl ++ [sHI] ++ b  ++ v0)
    pc' = lc_counter4  (cl ++ [sHI] ++ pc ++ v0)
    op' = lc_register4 (cl ++ [sHI] ++ op ++ v0)
