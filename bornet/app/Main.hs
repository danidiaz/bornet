module Main (main) where

import Control.Exception.Backtrace
import Bornet.Root qualified

main :: IO ()
main = do
  setBacktraceMechanismState IPEBacktrace True
  Bornet.Root.main
