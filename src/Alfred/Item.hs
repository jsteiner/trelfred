module Alfred.Item
    ( Item (..)
    , ToAlfredItem (..)
    ) where

import qualified Data.Csv as Csv
import Data.Csv ((.=), (.:))
import qualified Data.Text as T

data Item = Item
    { name :: T.Text
    , url :: T.Text
    , visits :: Int
    }

instance Csv.ToNamedRecord Item where
    toNamedRecord (Item name url visits) =
        Csv.namedRecord [ "name" .= name
                    , "url" .= url
                    , "visits" .= visits
                    ]

instance Csv.FromNamedRecord Item where
    parseNamedRecord m = Item
                          <$> m .: "name"
                          <*> m .: "url"
                          <*> m .: "visits"

instance Csv.DefaultOrdered Item where
    headerOrder _ = Csv.header ["name", "url", "visits"]

class ToAlfredItem a where
    toAlfredItem :: a -> Item
