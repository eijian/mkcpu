--
-- TD4 CPU
--

module Cpu (
  lc_td4
, lc_td4_dummy
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
lc_td4_dummy: for test of main routine
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

lc_td4_dummy :: LogicCircuit
lc_td4_dummy xs = concat [[sHI], a, b, pc, op]
  where
    rom = drop 18 xs
    a   = take 4 rom
    b   = take 4 (drop 4 rom)
    pc  = take 4 (drop 8 rom)
    op  = take 4 (drop 12 rom)
