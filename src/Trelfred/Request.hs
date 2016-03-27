module Trelfred.Request
    ( getBoards
    ) where

import System.Environment (getEnv)
import Control.Exception (throwIO)

import Data.Aeson (eitherDecode)
import qualified Data.Text as T
import Control.Lens ((.~), (^.), (&))
import LoadEnv (loadEnv)
import Network.Wreq

import Trelfred.Board

data Credentials = Credentials
    { apiKey :: String
    , apiToken :: String
    , username :: String
    }

getBoards :: IO [Board]
getBoards = do
    loadEnv
    c <- getCredentials
    let opts' = opts c
    let endpoint = boardsEndpoint $ username c
    requestBoards opts' endpoint

requestBoards :: Options -> String -> IO [Board]
requestBoards options endpoint = do
    r <- getWith options endpoint
    let json = r ^. responseBody
    let result = eitherDecode json :: Either String [Board]

    case result of
        Left e -> throwIO (userError e)
        Right boards -> return boards

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
