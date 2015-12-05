
import Logic.BasicGate

main :: IO ()
main = do
  putStrLn ("[L,L]->" ++ (show $ lc_and [False,False]))
  putStrLn ("[L,H]->" ++ (show $ lc_and [False,True]))
  putStrLn ("[H,L]->" ++ (show $ lc_and [True,False]))
  putStrLn ("[H,H]->" ++ (show $ lc_and [True,True]))

