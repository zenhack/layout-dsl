module Main where

import Test.Framework (defaultMain)
import Tests.Parser (parseTests)

main :: IO ()
main = defaultMain [parseTests]
