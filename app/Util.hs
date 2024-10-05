module Util where

import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine
