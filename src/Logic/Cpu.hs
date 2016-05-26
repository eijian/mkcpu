--
-- TD4 CPU
--

module Logic.Cpu (
  lc_td4
, lc_td4_st0
, lc_td4_st1
, lc_td4_st2
) where

import Logic.BasicGate
import Logic.ComplexLogic
import Logic.FlipFlop
import Logic.Register
import Logic.Rom

{- CONSTANT -}

zero :: [Bin]
zero = toBits "0000"   -- used to input zero value for adder

{- FUNCTIONS -}

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

{- |
TD4 CPU

  IN : [!C   clear (1),
        CF   carry flag (1),
        A    A register (4),
        B    B register (4),
        OP   output port (4),
        PC   program counter (4),
        IP   input port (4),
        ROM  ROM data 16 bytes (128)]
  OUT: [CF   carry flag (1),
        A    A register (4),
        B    B register (4),
        OP   output port (4),
        PC   program counter (4)
       ]
-}

{- lc_td4_st0: step 0 (through all inputs to outputs) -}

lc_td4_st0 :: LogicCircuit
lc_td4_st0 xs = concat [cf, a, b, op, pc]
  where
    [_, cf, a, b, op, pc, _, _] = splitInput xs

{- lc_td4_st1: step 1 (output via FlipFlop) -}

lc_td4_st1 :: LogicCircuit
lc_td4_st1 xs = concat [cf', a', b', op', pc']
  where
    [cl, cf, a, b, op, pc, _, _] = splitInput xs
    v0  = toBits "0000"
    cf' = take 1 $ lc_dff_cp (cl ++ [sHI] ++ cf)
    a'  = lc_register4 (cl ++ [sHI] ++ a  ++ v0)
    b'  = lc_register4 (cl ++ [sHI] ++ b  ++ v0)
    op' = lc_register4 (cl ++ [sHI] ++ op ++ v0)
    pc' = lc_counter4  (cl ++ [sHI] ++ pc ++ v0)

{- lc_td4_st2: step 2 (add immediate value to a) -}

lc_td4_st2 :: LogicCircuit
lc_td4_st2 xs = concat [cf', a', b', op', pc']
  where
    [cl, _, a, b, op, pc, _, rom] = splitInput xs
    rdata = lc_rom16 (pc ++ rom) -- get data addressed by PC
    v0  = toBits "0000"
    im  = take 4 rdata
    (s0, c0) = splitAt 4 $ lc_adder (a ++ im)
    cf' = take 1 $ lc_dff_cp (cl ++ [sHI] ++ c0)
    a'  = lc_register4 (cl ++ [sLO] ++ a  ++ s0)
    b'  = lc_register4 (cl ++ [sHI] ++ b  ++ v0)
    op' = lc_register4 (cl ++ [sHI] ++ op ++ v0)
    pc' = lc_counter4  (cl ++ [sHI] ++ pc ++ v0)

{- |
lc_td4: full TD4 cpu

>>> let rom0 = take ((16-1) * 8) $ repeat '0'
>>> -- ADD A,Im (A=1, Im=4 -> A=5, CF=0)
>>> toStr $ lc_td4 $ toBits ("10 1000 0000 0000 0000 0000 00100000" ++ rom0)
"01010000000001000"
>>> -- ADD A,Im (A=13, Im=4 -> A=1, CF=1)
>>> toStr $ lc_td4 $ toBits ("10 1011 0000 0000 0000 0000 00100000" ++ rom0)
"11000000000001000"
>>> -- MOV A,B (A=13, B=3 -> A=3)
>>> toStr $ lc_td4 $ toBits ("10 1011 1100 0000 0000 0000 00001000" ++ rom0)
"01100110000001000"
>>> -- IN A (A=13, IP=8 -> A=8)
>>> toStr $ lc_td4 $ toBits ("10 1011 0000 0000 0000 0001 00000100" ++ rom0)
"00001000000001000"
>>> -- MOV A,Im (A=13, Im=4 -> A=4)
>>> toStr $ lc_td4 $ toBits ("10 1011 0000 0000 0000 0000 00101100" ++ rom0)
"00010000000001000"
>>> -- MOV B,A (A=13, B=4 -> B=13)
>>> toStr $ lc_td4 $ toBits ("10 1011 0010 0000 0000 0000 00000010" ++ rom0)
"01011101100001000"
>>> -- ADD B,Im (B=1, Im=4 -> B=5, CF=0)
>>> toStr $ lc_td4 $ toBits ("10 0000 1000 0000 0000 0000 01001010" ++ rom0)
"00000110000001000"
>>> -- ADD B,Im (B=13, Im=4 -> B=1, CF=1)
>>> toStr $ lc_td4 $ toBits ("10 0000 1011 0000 0000 0000 00101010" ++ rom0)
"10000100000001000"
>>> -- IN B (B=13, IP=8 -> B=8)
>>> toStr $ lc_td4 $ toBits ("10 0000 1011 0000 0000 0001 00000110" ++ rom0)
"00000000100001000"
>>> -- MOV B,Im (B=13, Im=4 -> B=4)
>>> toStr $ lc_td4 $ toBits ("10 0000 1011 0000 0000 0000 00101110" ++ rom0)
"00000001000001000"
>>> -- OUT B (B=12 -> OP=12)
>>> toStr $ lc_td4 $ toBits ("10 0000 0011 0000 0000 0000 00001001" ++ rom0)
"00000001100111000"
>>> -- OUT Im (Im=12 -> OP=12)
>>> toStr $ lc_td4 $ toBits ("10 0000 0000 0000 0000 0000 00111101" ++ rom0)
"00000000000111000"
>>> -- OUT Im (PC=3, Im=4 -> OP=4, PC=4)
>>> toStr $ lc_td4 $ toBits ("10 0000 0000 0000 0000 0000 00111101" ++ rom0)
"00000000000111000"
>>> -- JNC Im (Im=12, CF=0 -> PC=12)
>>> toStr $ lc_td4 $ toBits ("10 0000 0000 0000 0000 0000 00110111" ++ rom0)
"00000000000000011"
>>> -- JNC Im (Im=12, CF=1 -> PC=1)
>>> toStr $ lc_td4 $ toBits ("11 0000 0000 0000 0000 0000 00110111" ++ rom0)
"00000000000001000"
>>> -- JMP Im (Im=12, CF=0 -> PC=12)
>>> toStr $ lc_td4 $ toBits ("10 0000 0000 0000 0000 0000 00111111" ++ rom0)
"00000000000000011"
>>> -- JMP Im (Im=12, CF=1 -> PC=12)
>>> toStr $ lc_td4 $ toBits ("11 0000 0000 0000 0000 0000 00111111" ++ rom0)
"00000000000000011"
>>> -- JMP Im (Im=12, CF=0, OP=3, PC=0 -> PC=12)
>>> toStr $ lc_td4 $ toBits ("10 0000 0000 1100 0000 0000 00111111" ++ rom0)
"00000000011000011"

-}

lc_td4 :: LogicCircuit
lc_td4 xs = concat [cf', a', b', op', pc']
  where
    [cl, cf, a, b, op, pc, ip, rom] = splitInput xs
    rdata = lc_rom16 (pc ++ rom)
    (im, inst) = splitAt 4 rdata
    [sa, sb, ld0, ld1, ld2, ld3] = lc_inst_decorder (inst ++ cf)
    (s0, c0) = splitAt 4 $ lc_adder ((selectInput [sa, sb] a b ip zero) ++ im)
    cf' = take 1 $ lc_dff_cp (cl ++ [sHI] ++ c0)
    a'  = lc_register4 (cl ++ [ld0] ++ a  ++ s0)
    b'  = lc_register4 (cl ++ [ld1] ++ b  ++ s0)
    op' = lc_register4 (cl ++ [ld2] ++ op ++ s0)
    pc' = lc_counter4  (cl ++ [ld3] ++ pc ++ s0)

{- split input -}

splitInput :: [Bin] -> [[Bin]]
splitInput xs = [cl, cf, a, b, op, pc, ip, rom]
  where
    (cl, xs0) = splitAt 1 xs
    (cf, xs1) = splitAt 1 xs0
    (a , xs2) = splitAt 4 xs1
    (b , xs3) = splitAt 4 xs2
    (op, xs4) = splitAt 4 xs3
    (pc, xs5) = splitAt 4 xs4
    (ip, rom) = splitAt 4 xs5

-- select data for adder input

{- |
selectInput

>>> let a  = toBits "1000"
>>> let b  = toBits "0110"
>>> let ip = toBits "0001"
>>> toStr $ selectInput [sLO, sLO] a b ip zero
"1000"
>>> toStr $ selectInput [sHI, sLO] a b ip zero
"0110"
>>> toStr $ selectInput [sLO, sHI] a b ip zero
"0001"
>>> toStr $ selectInput [sHI, sHI] a b ip zero
"0000"

-}

selectInput :: [Bin] -> [Bin] -> [Bin] -> [Bin] -> [Bin] -> [Bin]
selectInput s a b ip z = concat $ map (\x -> lc_multiplexer4ch (s ++ x)) mi
  where
    mi = buildMultiplexerInput [a, b, ip, z]

    buildMultiplexerInput :: [[Bin]] -> [[Bin]]
    buildMultiplexerInput xs = map (\i -> pickBit i xs) [0..3]

    pickBit :: Int -> [[Bin]] -> [Bin]
    pickBit i xs = map (!!i) xs
