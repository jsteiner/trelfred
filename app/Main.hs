module Main where

import Control.Monad (join)

import Options.Applicative

import Trelfred.Cache (cacheBoards)
import Trelfred.Search (searchBoards)

data Command
    = Search (Maybe String)
    | Cache

main :: IO ()
main = run =<< execParser
    (parseCommand `withInfo` "Interact with the Trello API")

run :: Command -> IO ()
run cmd =
    case cmd of
        Search mquery -> searchBoards mquery
        Cache         -> cacheBoards

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCommand :: Parser Command
parseCommand =
    subparser $
        command "search" (parseSearch `withInfo` "Search for Trello boards") <>
        command "cache"  (parseCache  `withInfo` "Cache Trello boards")

parseSearch :: Parser Command
parseSearch = Search <$> optional (argument str (metavar "QUERY"))

parseCache :: Parser Command
parseCache = pure Cache
