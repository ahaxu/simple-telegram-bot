module Main where

import EmaImproved (justDoIt)
import System.Environment (getArgs)
import Control.Monad.Trans.Except


main :: IO ()
main = do
  args <- getArgs
  let symbol = args!!0
  let interval = args!!1
  let backtracks = read $ args!!2 :: Int
  let threshold = read $ args !!3 :: Float

  result <- runExceptT $ justDoIt symbol interval backtracks threshold
  case result of
    Left e -> print e
    Right msg -> print msg

