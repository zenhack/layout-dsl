module Layout.Parser (pFile) where

import Control.Monad (void)
import Text.ParserCombinators.Parsec hiding(token)
import qualified Data.Text as T
import Layout.Ast


-- Misc. Utility stuff

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

-- | @token p@ parses @p@ with optional trailing whitespace/comments.
token :: Parser a -> Parser a
token p = p <* many whitespaceOrComment

-- | @keyword p@ parses the literal token @p@.
keyword :: String -> Parser ()
keyword = void . token . string

-- | Parse a whole source file
pFile = many whitespaceOrComment >> File <$> many pDecl

pIdent = token $ T.pack <$>
    ((:) <$> (letter <|> char '_')
         <*> many (letter <|> digit <|> char '_'))

pDecl = pTypeDecl

pTypeDecl = keyword "type" >> TypeDecl <$> pIdent <*> pType

pType :: Parser Type
pType = try pUIntType <|> try pStructType


pStructType = do
    keyword "struct" >> keyword "{"
    fields <- many1 $ do
        names <- pIdent `sepBy` (token $ char ',')
        keyword ":"
        ty <- pType
        return (names, ty)
    keyword "}"
    return $ StructT fields

pUIntType = do
    keyword "uint" >> keyword "<"
    num <- many1 digit
    keyword ">"
    return $ UIntT $ read num
