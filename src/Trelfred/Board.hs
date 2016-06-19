module Trelfred.Board
    ( Board (..)
    ) where

import Data.Monoid ((<>))

import Data.Aeson
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map

data Board = Board
    { boardId :: T.Text
    , boardName :: T.Text
    , boardUrl :: T.Text
    , boardColor :: Maybe T.Text
    } deriving (Show)

instance FromJSON Board where
    parseJSON = withObject "board" $ \b -> do
        bid <- b .: "id"
        name <- b .: "name"
        url <- b .: "url"
        prefs <- b .: "prefs"
        color <- prefs .:? "backgroundColor"
        pure $ Board bid name url color

instance ToJSON Board where
    toJSON (Board bid name url color) =
        object
            [ "uid" .= bid
            , "title" .= name
            , "arg" .= url
            , "icon" .= toIcon color
            ]

toIcon :: Maybe T.Text -> Map.HashMap T.Text T.Text
toIcon color = Map.fromList [ ("path", toIconPath color) ]

toIconPath :: Maybe T.Text -> T.Text
toIconPath (Just color) = "icons/" <> T.drop 1 color <> ".png"
toIconPath Nothing = toIconPath $ Just defaultColor

defaultColor :: T.Text
defaultColor = "#0079BF"
