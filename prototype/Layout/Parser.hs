module Layout.Parser where

import Control.Monad (void)
import Text.ParserCombinators.Parsec hiding(token)
import qualified Data.Text as T
import Layout.Ast

whitespace, comment, blockComment, eolComment :: Parser ()

whitespace = void $ oneOf " \t\r\n\f\v"
comment = try eolComment <|> blockComment
eolComment = do
    string "// "
    void $ many (noneOf "\n")
blockComment = do
    string "/*"
    void $ manyTill anyChar (try $ string "*/")
whitespaceOrComment = try comment <|> whitespace

token :: Parser a -> Parser a
token p = p <* many whitespaceOrComment

pIdent = token $ T.pack <$>
    ((:) <$> (letter <|> char '_')
         <*> many (letter <|> digit <|> char '_'))

pLayoutK = token (string "layout")
pTypeK = token (string "type")
pStructK = token (string "struct")

pDecl = pTypeDecl

pTypeDecl = pTypeK >> TypeDecl <$> pIdent <*> pType

pType :: Parser Type
pType = try pUIntType <|> pStructType

pStructType = do
    pStructK
    token $ char '{'
    fields <- many1 $ do
        names <- pIdent `sepBy` (token $ char ',')
        token (char ':')
        ty <- pType
        return (names, ty)
    token $ char '}'
    return $ StructT fields

pUIntType = do
    token $ string "uint"
    token $ char '<'
    num <- many1 digit
    token $ char '>'
    return $ UIntT $ read num
