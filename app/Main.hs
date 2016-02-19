module Main where

import Control.Monad (join)

import Options.Applicative

import Cache (cacheBoards)
import Search (searchBoards)

args :: Parser (IO ())
args =
    subparser $
        command "search" (info (searchBoards <$> argument str (metavar "QUERY")) idm) <>
        command "cache"  (info (pure cacheBoards) (progDesc "foo"))

main :: IO ()
main = join $ execParser (info args idm)
