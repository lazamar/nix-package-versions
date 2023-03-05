module System.Timed (timed) where

import System.TimeIt (timeItT)
import System.IO (hPutStrLn, stderr)

timed :: String -> IO b -> IO b
timed msg action = do
  (time, result) <- timeItT action
  hPutStrLn stderr $ "[" <> show time <> "] " <> msg
  return result
