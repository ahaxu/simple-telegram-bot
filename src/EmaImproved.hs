{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  DeriveGeneric #-}

module EmaImproved where

import           Data.Aeson
                    ( Value
                    , encode
                    , Array
                    , FromJSON
                    , ToJSON
                    , Object
                    )
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple
import           Control.Exception
                    ( try
                    , Exception
                    , SomeException
                    )
import           Network.HTTP.Client
                    (HttpExceptionContent(StatusCodeException))
import           Data.Typeable
import           Data.Aeson.Types (parseEither)
import           Text.Read.Lex (Number)
import           GHC.Generics
import           System.Environment (getEnv)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class


data MyException =
    MyException String
        deriving (Show, Typeable)
instance Exception MyException

data Candle =
    Candle {
          backtrack :: Int
        , value :: Float
    } deriving (Show, Generic)

type Candles = [Candle]

instance FromJSON Candle
instance ToJSON Candle

getEma ::
       String
    -> String
    -> Int
    -> ExceptT MyException IO Candles
getEma symbol interval backtracks = do
    key <-  lift $ getEnv "TA_API_KEY"
    ExceptT $ do
        let params =
                [ ("secret", Just $ S8.pack key)
                , ("exchange", Just "binance")
                , ("symbol", Just $ S8.pack symbol)
                , ("interval", Just $ S8.pack interval)
                , ("backtracks", Just $ S8.pack $ show backtracks)
                ]
            request =
                  setRequestPath "/ema"
                $ setRequestHost "api.taapi.io"
                $ setRequestMethod "GET"
                $ setRequestQueryString params
                $ setRequestSecure True
                $ setRequestPort 443
                $ defaultRequest
        response <- try $ httpJSON request
        case response of
            Left err -> return $ Left err
            Right r ->  return $
                if statusCode == 200
                    then Right $ getResponseBody r
                    else Left $ MyException $ L8.unpack $ encode rBody
                where
                    statusCode = getResponseStatusCode r
                    rBody = getResponseBody r :: Candles

data TelegramMessage = TelegramMessage
    {
          symbol :: String
        , trend :: String
        , percentage :: Float
        , base :: Float
        , gap :: Float
        , firstCandleValue :: Float
        , lastCandleValue :: Float
    } deriving(Show)

handleEma ::
    String
    -> Float
    -> Candles
    -> ExceptT MyException IO String
handleEma symbol threshold cdls = ExceptT $ do
    if percentageValue >= threshold
        then return $ Right $ show msg
        else return $ Left $ MyException $ "Not reach threshold yet" ++ "; percentageValue " ++ show percentageValue ++ " | threshold "++ show threshold
    where
        firstCandleValue = value $ head cdls
        lastCandleValue  = value $ last cdls
        gap = firstCandleValue - lastCandleValue
        baseValue
            | gap > 0 = lastCandleValue
            | otherwise = firstCandleValue
        percentageValue = abs gap/baseValue * 100
        msg
            | gap > 0 = TelegramMessage
                { symbol=symbol
                , trend="down"
                , percentage=percentageValue
                , base=baseValue
                , gap=abs gap
                , firstCandleValue=firstCandleValue
                , lastCandleValue=lastCandleValue
                }
            | otherwise = TelegramMessage
                { symbol=symbol
                , trend="up"
                , percentage=percentageValue
                , base=baseValue
                , gap=abs gap
                , firstCandleValue=firstCandleValue
                , lastCandleValue=lastCandleValue
                }

data TelegramResponseData= TelegramResponseData
    {
          ok :: Bool
        , result :: Object
    } deriving(Show, Generic)

instance FromJSON TelegramResponseData
instance ToJSON TelegramResponseData

sendToTelegram ::
       String
    -> ExceptT MyException IO TelegramResponseData
sendToTelegram msg = do
    telegramKey <- lift $ getEnv "TELEGRAM_BOT_KEY"
    telegramChatID <- lift $ getEnv "CHAT_ID"
    let params =
            [ ("chat_id", Just $ S8.pack telegramChatID)
            , ("text", Just $ S8.pack msg)
            ]
        requestPath = S8.pack $ "/" ++ telegramKey ++ "/sendmessage"
        request =
              setRequestMethod "GET"
            $ setRequestHost "api.telegram.org"
            $ setRequestPath requestPath
            $ setRequestQueryString params
            $ setRequestSecure True
            $ setRequestPort 443
            $ defaultRequest
    ExceptT $ do
        response <- try $ httpJSON request
        case response of
            Left err -> return $ Left err
            Right r ->  return $
                if statusCode == 200
                    then Right $ getResponseBody r
                    else Left $ MyException $ L8.unpack $ encode rBody
                where
                    statusCode = getResponseStatusCode r
                    rBody = getResponseBody r :: TelegramResponseData

justDoIt ::
       String
    -> String
    -> Int
    -> Float
    -> ExceptT MyException IO String
justDoIt symbol interval backtracks threshold = do
    candles <- getEma symbol interval backtracks
    emaSignal <- handleEma symbol threshold candles
    teleMsg <- sendToTelegram emaSignal
    return $ show teleMsg

