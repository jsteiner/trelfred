module Alfred.Increment
    ( incrementVisits
    ) where

import qualified Data.Text as T

import qualified Data.Vector as V

import qualified Alfred.Item as A
import Alfred.Search
import Alfred.Cache

incrementVisits :: String -> T.Text -> IO ()
incrementVisits cacheFile url = do
    items <- getItems cacheFile
    let items' = foldr (incrementIfItem url) [] items
    writeItems cacheFile items'

incrementIfItem :: T.Text -> A.Item -> [A.Item] -> [A.Item]
incrementIfItem url item items
    | url == A.url item = increment item : items
    | otherwise = item : items

increment :: A.Item -> A.Item
increment r =
    r { A.visits = (+1) $ A.visits r }
