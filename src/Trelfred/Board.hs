module Trelfred.Board
    ( Board (..)
    ) where

import Control.Monad (mzero)

import Data.Aeson
import qualified Data.Text as T
import qualified Alfred.Item as A

data Board = Board
    { boardName :: T.Text
    , boardUrl :: T.Text
    , visits :: Int
    } deriving (Show)

instance FromJSON Board where
    parseJSON (Object v) = do
        name <- v .: "name"
        url <- v .: "url"

        return $ Board name url 0

    parseJSON _ = mzero

instance A.ToAlfredItem Board where
    toAlfredItem (Board name url visits) = A.Item name url visits
