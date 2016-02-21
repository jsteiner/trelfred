module Trelfred.Search
    ( searchBoards
    , getBoards
    ) where

import Data.Maybe (fromMaybe)
import Data.Function (on)
import Control.Exception (throwIO)
import qualified Data.List as L

import Data.Csv (decodeByName, Header)
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Text.XML.Generator
import qualified Text.Fuzzy as Fuzzy

import Trelfred.Cache (cacheFile)
import Trelfred.Board

searchBoards :: Maybe String -> IO ()
searchBoards mq = do
    boards <- getBoards
    printXML . sortByHits . V.toList . matchingBoards boards $ mq

sortByHits :: [Board] -> [Board]
sortByHits = L.sortBy comparator
    where
        comparator = flip compare `Data.Function.on` visits

getBoards :: IO (V.Vector Board)
getBoards = do
    json <- BS.readFile cacheFile
    let result = decodeByName json :: Either String (Header, V.Vector Board)
    case result of
        Left e -> throwIO $ userError e
        Right (_, boards) -> return boards

matchingBoards :: V.Vector Board -> Maybe String -> V.Vector Board
matchingBoards boards Nothing = boards
matchingBoards boards (Just q) = V.filter (boardMatches $ T.pack q) boards

printXML :: [Board] -> IO ()
printXML = BS.putStr . xrender . boardsToXML

boardsToXML :: [Board] -> Xml Elem
boardsToXML bs =
    xelem "items" $
        xelems $ boardToElem <$> bs

boardToElem :: Board -> Xml Elem
boardToElem b =
    xelem "item" (attr, elem)
    where
        attr = xattr "arg" $ boardUrl b
        elem = xelem "title" $ xtext $ boardName b

boardMatches :: T.Text -> Board -> Bool
boardMatches q b = Fuzzy.test q $ boardName b
