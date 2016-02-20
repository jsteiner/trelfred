module Search
    ( searchBoards
    ) where

import Control.Monad (mzero)
import Data.Maybe (fromMaybe)

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Text.XML.Generator
import qualified Text.Fuzzy as Fuzzy

data Board = Board
    { boardName :: T.Text
    , boardId :: T.Text
    , boardUrl :: T.Text
    } deriving (Show)

instance FromJSON Board where
    parseJSON (Object v) = Board
                            <$> v .: "name"
                            <*> v .: "id"
                            <*> v .: "url"
    parseJSON _          = mzero

searchBoards :: Maybe String -> IO ()
searchBoards mq = do
    json <- BS.readFile "boards.json"
    let mboards = decode json
    printXML $ matchingBoards mboards mq

matchingBoards :: Maybe [Board] -> Maybe String -> [Board]
matchingBoards Nothing _ = []
matchingBoards (Just boards) Nothing = boards
matchingBoards (Just boards) (Just q) = filter (boardMatches $ T.pack q) boards

printXML :: [Board] -> IO ()
printXML = BS.putStr . xrender . boardsToXML

boardsToXML :: [Board] -> Xml Elem
boardsToXML bs =
    xelem "items" $
        xelems $ boardToElem <$> bs

boardToElem :: Board -> Xml Elem
boardToElem b = xelem "item" (attr, elem)
    where
        attr = xattr "arg" $ boardUrl b
        elem = xelem "title" $ xtext $ boardName b

boardMatches :: T.Text -> Board -> Bool
boardMatches q b = Fuzzy.test q $ boardName b
