module Main where

import Layout.Parser
import Text.ParserCombinators.Parsec (runParser)

main :: IO ()
main = print (runParser pDecl () "" "type Foo uint<71>")
