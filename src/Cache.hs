module Cache
    ( cacheBoards
    ) where

import System.Environment (getEnv)

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import Control.Lens ((.~), (^.), (&))
import LoadEnv (loadEnv)
import Network.Wreq

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
             & param "token"   .~ [T.pack apiToken]

boardsEndpoint :: String -> String
boardsEndpoint u =
    "https://api.trello.com/1/members/" ++ u ++ "/boards?filter=open"

writeResponse :: Options -> String -> IO ()
writeResponse o e = do
    r <- getWith o e
    let json = r ^. responseBody
    BS.writeFile "boards.json" json
