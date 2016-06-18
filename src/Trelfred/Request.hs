module Trelfred.Request
    ( cacheBoards
    ) where

import System.Environment (getEnv)
import Control.Exception (throwIO)

import Control.Lens ((.~), (^.), (&))
import qualified Network.Wreq as W
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as Map

import Trelfred.Board

data Credentials = Credentials
    { _apiKey :: String
    , _apiToken :: String
    , username :: String
    }

cacheBoards :: IO ()
cacheBoards = do
    boards <- getBoards
    let jsonOut = Aeson.encode $ wrapBoards boards
    BS.writeFile cacheFile jsonOut

wrapBoards :: [Board] -> Map.HashMap T.Text [Board]
wrapBoards bs = Map.fromList [ ("items", bs) ]

getBoards :: IO [Board]
getBoards = do
    c <- getCredentials
    let opts' = opts c
    let endpoint = boardsEndpoint $ username c
    requestBoards opts' endpoint

requestBoards :: W.Options -> String -> IO [Board]
requestBoards options endpoint = do
    r <- W.getWith options endpoint
    let json = r ^. W.responseBody
    let result = Aeson.eitherDecode json :: Either String [Board]

    case result of
        Left e -> throwIO (userError e)
        Right boards -> return boards

getCredentials :: IO Credentials
getCredentials =
    Credentials
        <$> getEnv "TRELLO_API_KEY"
        <*> getEnv "TRELLO_API_TOKEN"
        <*> getEnv "TRELLO_USERNAME"

opts :: Credentials -> W.Options
opts (Credentials apiKey apiToken _) =
    W.defaults & W.param "key"   .~ [T.pack apiKey]
               & W.param "token" .~ [T.pack apiToken]

boardsEndpoint :: String -> String
boardsEndpoint u =
    "https://api.trello.com/1/members/" ++ u ++ "/boards?filter=open"

cacheFile :: String
cacheFile = "cache.json"
