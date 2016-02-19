module Search
    ( searchBoards
    ) where

import Control.Monad (mzero)
import Data.Maybe (fromMaybe)
import Data.List as List

import Data.Aeson
import Data.ByteString.Lazy as BS
import Data.Text as T
import Text.XML.Generator

data Board = Board
    { boardName :: Text
    , boardId :: Text
    , boardUrl :: Text
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
    let mmatching = matchingBoards mboards mq
    printXML mmatching

matchingBoards :: Maybe [Board] -> Maybe String -> [Board]
matchingBoards Nothing _ = []
matchingBoards (Just boards) Nothing = boards
matchingBoards (Just boards) (Just q) = List.filter (boardMatches $ T.pack q) boards

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

boardMatches :: Text -> Board -> Bool
boardMatches q b = q' `T.isInfixOf` name
    where
      q' = toUpper q
      name = toUpper $ boardName b

