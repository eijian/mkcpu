
import Logic.BasicGate

main :: IO ()
main = do
  putStrLn ("[L,L]->" ++ (show $ lc_and [sLO,sLO]))
  putStrLn ("[L,H]->" ++ (show $ lc_and [sLO,sHI]))
  putStrLn ("[H,L]->" ++ (show $ lc_and [sHI,sLO]))
  putStrLn ("[H,H]->" ++ (show $ lc_and [sHI,sHI]))

