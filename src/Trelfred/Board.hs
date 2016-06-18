module Trelfred.Board
    ( Board (..)
    ) where

import Control.Monad (mzero)

import Data.Aeson
import qualified Data.Text as T

data Board = Board
    { boardId :: T.Text
    , boardName :: T.Text
    , boardUrl :: T.Text
    } deriving (Show)

instance FromJSON Board where
    parseJSON (Object b) =
        Board
        <$> b .: "id"
        <*> b .: "name"
        <*> b .: "url"
    parseJSON _ = mzero

instance ToJSON Board where
    toJSON (Board bid name url) =
        object
            [ "uid" .= bid
            , "title" .= name
            , "arg" .= url
            ]
