module Main where

import           Cli (getCliCommand)

main :: IO ()
main = do
    getCliCommand >>= \result -> print result
