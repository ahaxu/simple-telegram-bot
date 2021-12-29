module Main where

import Ema (getEma, handleEma, sendToTelegram)
import System.Environment (getArgs)

main :: IO ()
main = do
  -- todo pass through arg
  args <- getArgs
  let symbol = args!!0
  let interval = args!!1
  let backtracks = read $ args!!2 :: Int
  let threshold = read $ args !!3 :: Float
  candles <- getEma symbol interval backtracks
  case candles of
    Left e -> print e
    Right cdls -> do
        handler <- handleEma symbol threshold cdls
        case handler of
            Left e -> print e
            Right msg -> do
                rs <- sendToTelegram msg
                case rs of
                    Left e -> print e
                    Right telegramRes -> print $ show telegramRes
