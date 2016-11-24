module Main where

import Layout.Parser
import Text.ParserCombinators.Parsec (runParser)

main :: IO ()
main = do
    contents <- getContents
    print (runParser pFile () "" contents)
