module Alfred.Search
    ( searchItems
    , getItems
    ) where

import Data.Maybe (fromMaybe)
import Data.Function (on)
import Control.Exception (throwIO)
import qualified Data.List as L
import System.Directory (doesFileExist)

import qualified Data.Csv as Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Text.XML.Generator
import qualified Text.Fuzzy as Fuzzy

import qualified Alfred.Item as A

searchItems :: String -> Maybe String -> IO ()
searchItems cacheFile mq = do
    items <- getItems cacheFile
    printXML . sortByHits . V.toList . matchingItems items $ mq

sortByHits :: [A.Item] -> [A.Item]
sortByHits = L.sortBy comparator
    where
        comparator = flip compare `Data.Function.on` A.visits

getItems :: String -> IO (V.Vector A.Item)
getItems cacheFile = do
    csvText <- BS.readFile cacheFile
    let result = Csv.decodeByName csvText
    case result of
        Left e -> throwIO $ userError e
        Right (_, items) -> return items

matchingItems :: V.Vector A.Item -> Maybe String -> V.Vector A.Item
matchingItems items Nothing = items
matchingItems items (Just q) = V.filter (itemMatches $ T.pack q) items

printXML :: [A.Item] -> IO ()
printXML = BS.putStr . xrender . itemsToXML

itemsToXML :: [A.Item] -> Xml Elem
itemsToXML rs =
    xelem "items" $
        xelems $ itemToElem <$> rs

itemToElem :: A.Item -> Xml Elem
itemToElem r =
    xelem "item" (attr, elem)
  where
    attr = xattr "arg" $ A.url r
    elem = xelem "title" $ xtext $ A.name r

itemMatches :: T.Text -> A.Item -> Bool
itemMatches q = Fuzzy.test q . A.name
