module Alfred.Cache
    ( writeItems
    ) where

import Data.Csv (encodeDefaultOrderedByName)
import qualified Data.ByteString.Lazy as BS

import qualified Alfred.Item as A

writeItems :: String -> [A.Item] -> IO ()
writeItems cacheFile = BS.writeFile cacheFile . encodeDefaultOrderedByName
