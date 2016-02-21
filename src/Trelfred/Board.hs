module Trelfred.Board
    ( Board (..)
    ) where

import Control.Monad (mzero)

import Data.Aeson
import qualified Data.Text as T

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
