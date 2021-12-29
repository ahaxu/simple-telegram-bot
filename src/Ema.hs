{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  DeriveGeneric #-}

module Ema where

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
import System.Environment (getEnv)


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
    -> IO (Either MyException Candles)
getEma symbol interval backtracks = do
    secret <- try $ getEnv "TA_API_KEY"
    case secret of
        Left e -> return $ Left e
        Right key -> do
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


handleEma :: String -> Float -> Candles -> IO (Either MyException String)
handleEma symbol threshold cdls = do
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
    -> IO (Either MyException TelegramResponseData)
sendToTelegram msg = do
    keyEnv <- try $ getEnv "TELEGRAM_BOT_KEY"
    case keyEnv of
        Left e -> return $ Left e
        Right key -> do
            chatIDEnv <- try $ getEnv "CHAT_ID"
            case chatIDEnv of
                Left e -> return $ Left e
                Right chatID -> do
                    let params =
                            [ ("chat_id", Just $ S8.pack chatID)
                            , ("text", Just $ S8.pack msg)
                            ]
                        requestPath = S8.pack $ "/" ++ key ++ "/sendmessage"
                        request =
                              setRequestMethod "GET"
                            $ setRequestHost "api.telegram.org"
                            $ setRequestPath requestPath
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
                                rBody = getResponseBody r :: TelegramResponseData

