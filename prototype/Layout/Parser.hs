module Layout.Parser (pFile) where

import Control.Monad (void)
import Data.Bits(shiftL)
import Text.ParserCombinators.Parsec hiding(token)
import qualified Data.Text as T
import Data.Text (Text)
import Layout.Ast


pLetter, pBinaryDigit, pDecimalDigit, pOctalDigit, pHexDigit :: Parser Char

pLetter = oneOf $ ['A'..'Z'] ++ ['a'..'z'] ++ ['_']
pBinaryDigit = oneOf "01"
pDecimalDigit = oneOf ['0'..'9']
pOctalDigit = oneOf ['0'..'7']
pHexDigit = oneOf $ ['0'..'9'] ++ ['A'..'F'] ++ ['a'..'f']

-- Misc. stuff to ignore:

whitespace, comment, blockComment, eolComment :: Parser ()

whitespace = void $ oneOf " \t\r\n\f\v"
comment = try eolComment <|> blockComment
eolComment = do
    string "//"
    void $ many (noneOf "\n")
blockComment = do
    string "/*"
    void $ manyTill anyChar (try $ string "*/")
whitespaceOrComment = comment <|> try whitespace


-- | @token p@ parses @p@ with optional trailing whitespace/comments.
token :: Parser a -> Parser a
token p = p <* many whitespaceOrComment

-- | @keyword p@ parses the literal token @p@.
keyword :: String -> Parser ()
keyword = void . token . string

pIdent :: Parser Text
pIdent = try $ token $ do
    first <- pLetter
    rest <- many (pLetter <|> pDecimalDigit)
    let name = first:rest
    if name `elem` ["layout", "struct", "type", "uint", "bool"] then
        pzero
    else
        return (T.pack name)

{-


pIdent = try $ maybeID $ token $ T.pack <$>
    -- TODO: Don't accept keywords.
    ((:) <$> (letter <|> char '_')
         <*> many (letter <|> digit <|> char '_'))
  where
    maybeID p = do
        name <- p
        if name `elem` ["layout", "struct", "type", "uint", "bool"] then
            pzero
        else
            return name
-}

pIntLit :: Parser Int
pIntLit = interpIntLit <$> (token $ choice [ pDecimalLit
                                           , pOctalLit
                                           , pHexLit
                                           , pBinaryLit
                                           ])
pDecimalLit = (:) <$> oneOf ['1'..'9'] <*> many pDecimalDigit
pOctalLit = (++)
    <$> pLitPrefix "oO"
    <*> many1 pOctalDigit
pHexLit = (++)
    <$> pLitPrefix "xX"
    <*> many1 pHexDigit
pBinaryLit = (++)
    <$> pLitPrefix "bB"
    <*> many1 pBinaryDigit

pLitPrefix :: String -> Parser String
pLitPrefix radix = do
    char '0'
    r <- oneOf radix
    return ['0', r]

getRadix :: Char -> Int
getRadix r
    | r `elem'` "oO" = 8
    | r `elem'` "xX" = 16
    | r `elem'` "bB" = 2
    | otherwise = error $ "invalid radix: " ++ [r]
  where
    -- Supriing to me, but GHC fails to infer the types above without this:
    elem' :: Char -> String -> Bool
    elem' = elem

interpIntLit :: String -> Int
interpIntLit lit@('1':_) = read lit
interpIntLit lit@('0':r:ds) = case getRadix r of
    8 -> read ('0':ds)
    16 ->  read lit
    2 -> let digits = map (read . (:[])) ds
         in sum $ zipWith shiftL (reverse digits) [0,1..]
interpIntLit ds =
    error $ "BUG invalid integer literal passed to interpIntLit: " ++ ds


pConstField :: Parser LayoutField
pConstField = try pConstRadixField <|> pConstDecimalField
pConstRadixField = do
    width <- interpIntLit <$> pDecimalLit
    char '\''
    rest <- pOctalLit <|> pHexLit <|> pBinaryLit
    let radix = getRadix (head rest)
    let value = interpIntLit rest
    return $ FixedL width radix value
pConstDecimalField = do
    width <- pDecimalLit
    char '\''
    pLitPrefix "dD"
    value <- interpIntLit <$> pDecimalLit
    return $ FixedL (interpIntLit width) 10 value


-- | Parse a whole source file
pFile = many whitespaceOrComment >> File <$> many pDecl


pDecl :: Parser Decl
pDecl = try pTypeDecl <|> pLayoutDecl

pTypeDecl :: Parser Decl
pTypeDecl = keyword "type" >> TypeDecl <$> pIdent <*> pType

pType :: Parser Type
pType = try pUIntType <|> pStructType


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

pLayoutDecl :: Parser Decl
pLayoutDecl = do
    keyword "layout"
     -- TODO: layout params
    name <- pIdent
    keyword "{"
    specs <- many pLayoutSpec
    keyword "}"
    return (LayoutDecl name [] specs)

pLayoutSpec = LayoutSpec [] <$> pLayoutField


pLayoutField :: Parser LayoutField
pLayoutField = choice $ map try
    [ do
        name <- pIdent
        keyword "["
        index1 <- read <$> many1 digit
        keyword ":"
        index2 <- read <$> many1 digit
        keyword "]"
        return $ SliceL name index1 index2
    , WholeL <$> pIdent
    ]
