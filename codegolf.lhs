find abundant numbers

import System.Environment

main = do
  mapM print [n|n<-[1..200],sum [d|d<-[1..(n `div` 2)],n `mod` d== 0]>n]
  args <- tail <$> getArgs
  mapM putStrLn args