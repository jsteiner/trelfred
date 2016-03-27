module Main where

import Control.Monad (join)

import Options.Applicative
import qualified Data.Text as T

import Trelfred.Request
import Alfred.Item
import Alfred.Search
import Alfred.Increment
import Alfred.Cache

data Command
    = Search (Maybe String)
    | Cache
    | Increment String

main :: IO ()
main = run =<< execParser
    (parseCommand `withInfo` "Interact with the Trello API")

run :: Command -> IO ()
run cmd =
    case cmd of
        Search mquery -> searchItems cacheFile mquery
        Cache         -> do
            boards <- getBoards
            writeItems cacheFile $ fmap toAlfredItem boards
        Increment url -> incrementVisits cacheFile $ T.pack url

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCommand :: Parser Command
parseCommand =
    subparser $
        command "search"     (parseSearch `withInfo` "Search for Trello boards") <>
        command "cache"      (parseCache  `withInfo` "Cache Trello boards") <>
        command "increment"  (parseIncrement  `withInfo` "Increment a board's visit count")

parseSearch :: Parser Command
parseSearch = Search <$> optional (argument str (metavar "QUERY"))

parseCache :: Parser Command
parseCache = pure Cache

parseIncrement :: Parser Command
parseIncrement = Increment <$> argument str (metavar "BOARD_URL")

cacheFile :: String
cacheFile = "boards.csv"
