
module Asm.CodeGenerator where

import Text.Parsec
import Asm.Mnemonic

generate :: Either ParseError [Mnemonic] -> [String]
generate (Left s)       = [show s]
generate (Right [])     = []
generate (Right (x:xs)) = (translateOne x):(generate $ Right xs)

translateOne :: Mnemonic -> String
translateOne (Add, (RegA, Just (Imdata s))) = "0000" ++ s
translateOne (Mov, (RegA, Just RegB))       = "00010000"
translateOne (In , (RegA, Nothing))         = "00100000"
translateOne (Mov, (RegA, Just (Imdata s))) = "0011" ++ s
translateOne (Mov, (RegB, Just RegA))       = "01000000"
translateOne (Add, (RegB, Just (Imdata s))) = "0101" ++ s
translateOne (In , (RegB, Nothing))         = "01100000"
translateOne (Mov, (RegB, Just (Imdata s))) = "0111" ++ s
translateOne (Out, (RegB, Nothing))         = "10010000"
translateOne (Out, (Imdata s, Nothing))     = "1011" ++ s
translateOne (Jnc, (Imdata s, Nothing))     = "1110" ++ s
translateOne (Jmp, (Imdata s, Nothing))     = "1111" ++ s
translateOne _                              = error "no such mnemonic"

