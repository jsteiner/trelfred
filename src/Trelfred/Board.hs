module Trelfred.Board
    ( Board (..)
    ) where

import Control.Monad (mzero)

import Data.Aeson hiding ((.=))
import qualified Data.Csv as Csv
import qualified Data.Text as T

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

    parseJSON _          = mzero

instance Csv.ToNamedRecord Board where
    toNamedRecord (Board name url visits) =
        Csv.namedRecord [ "name" Csv..= name
                        , "url" Csv..= url
                        , "visits" Csv..= visits
                        ]

instance Csv.FromNamedRecord Board where
    parseNamedRecord m = Board
                          <$> m Csv..: "name"
                          <*> m Csv..: "url"
                          <*> m Csv..: "visits"

instance Csv.DefaultOrdered Board where
    headerOrder _ = Csv.header ["name", "url", "visits"]
