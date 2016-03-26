module Trelfred.Cache
    ( cacheBoards
    , cacheFile
    , writeBoards
    ) where

import System.Environment (getEnv)
import Control.Exception (throwIO)

import Data.Aeson (eitherDecode)
import Data.Csv (encodeDefaultOrderedByName)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import Control.Lens ((.~), (^.), (&))
import LoadEnv (loadEnv)
import Network.Wreq

import Trelfred.Board

data Credentials = Credentials
    { apiKey :: String
    , apiToken :: String
    , username :: String
    }

cacheBoards :: IO ()
cacheBoards = do
    loadEnv
    c <- getCredentials
    let opts' = opts c
    let endpoint = boardsEndpoint $ username c
    writeResponse opts' endpoint

getCredentials :: IO Credentials
getCredentials =
    Credentials
        <$> getEnv "TRELLO_API_KEY"
        <*> getEnv "TRELLO_API_TOKEN"
        <*> getEnv "TRELLO_USERNAME"

opts :: Credentials -> Options
opts (Credentials apiKey apiToken _) =
    defaults & param "key"   .~ [T.pack apiKey]
             & param "token" .~ [T.pack apiToken]

boardsEndpoint :: String -> String
boardsEndpoint u =
    "https://api.trello.com/1/members/" ++ u ++ "/boards?filter=open"

cacheFile :: String
cacheFile = "boards.csv"

writeResponse :: Options -> String -> IO ()
writeResponse options endpoint = do
    r <- getWith options endpoint
    let json = r ^. responseBody
    let result = eitherDecode json :: Either String [Board]
    case result of
        Left e -> throwIO (userError e)
        Right boards -> writeBoards boards

writeBoards :: [Board] -> IO ()
writeBoards = BS.writeFile cacheFile . encodeDefaultOrderedByName
