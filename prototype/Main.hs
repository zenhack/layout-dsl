module Main where

import Language.LDSL.Ast (File(..))
import Language.LDSL.Parser
import Language.LDSL.Validate (buildSyms)
import Text.ParserCombinators.Parsec (runParser)

import Language.LDSL.IR.RangeMap

main :: IO ()
main = do
    contents <- getContents
    case runParser pFile () "<stdin>" contents of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right (File decls)-> case buildSyms decls of
            Left err -> putStrLn $ "validation error: " ++ show err
            ok@(Right _) -> print ok
