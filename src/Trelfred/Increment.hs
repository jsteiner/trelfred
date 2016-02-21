module Trelfred.Increment
    ( incrementVisits
    ) where

import qualified Data.Text as T

import qualified Data.Vector as V

import Trelfred.Board
import Trelfred.Search (getBoards)
import Trelfred.Cache (writeBoards)

incrementVisits :: T.Text -> IO ()
incrementVisits url = do
    boards <- getBoards
    let boards' = foldr (incrementIfBoard url) [] boards
    writeBoards boards'

incrementIfBoard :: T.Text -> Board -> [Board] -> [Board]
incrementIfBoard url board boards
    | url == boardUrl board = increment board : boards
    | otherwise = board : boards

increment :: Board -> Board
increment b =
    b { visits = (+1) $ visits b }
