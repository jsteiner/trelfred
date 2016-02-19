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

searchBoards :: String -> IO ()
searchBoards q = do
    json <- BS.readFile "boards.json"
    let mboards = decode json
    let q' = T.pack q
    let mmatching = List.filter (boardMatches q') <$> mboards
    printXML mmatching

printXML :: Maybe [Board] -> IO ()
printXML = BS.putStr . xrender . boardsToXML . fromMaybe []

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

