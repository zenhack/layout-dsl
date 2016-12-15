--module Layout.Parser (pFile) where
module Layout.Parser where

-- General note: we make use of fewer of parsec's features and pre-existing
-- helpers than might otherwise sense, favoring instead having this
-- implementation closely follow the grammar in ../grammar.md. This should make
-- it easier to verify that the implementation is correct.

-- These are in Prelude on more recent versions of base, but not older ones; we
-- may as well import them:
import Control.Applicative ((<$>), (<*), (<*>), (*>))

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
pIdent = (try $ token $ do
    first <- pLetter
    rest <- many (pLetter <|> pDecimalDigit)
    let name = first:rest
    if name `elem` ["layout", "struct", "type", "uint", "bool"] then
        pzero
    else
        return (T.pack name)) <?> "identifier"


pIntLit :: Parser Int
pIntLit = interpIntLit <$>
    ((token $ choice $ map try [ pDecimalLit
                               , pHexLit
                               , pBinaryLit
                               , pOctalLit
                               ]) <?> "integer literal")
pDecimalLit = pNoRadixDecimalLit <|> pRadixDecimalLit where
    pNoRadixDecimalLit = (:)  <$> oneOf ['1'..'9'] <*> many  pDecimalDigit
    pRadixDecimalLit   = (++) <$> pLitPrefix "dD"  <*> many1 pDecimalDigit
pHexLit    = (++) <$> pLitPrefix "xX" <*> many1 pHexDigit
pBinaryLit = (++) <$> pLitPrefix "bB" <*> many1 pBinaryDigit
pOctalLit = try pRadixOctalLit <|> pNoRadixOctalLit where
    pNoRadixOctalLit = (:)  <$> char '0'        <*> many  pOctalDigit
    pRadixOctalLit   = (++) <$> pLitPrefix "oO" <*> many1 pOctalDigit

pLitPrefix :: String -> Parser String
pLitPrefix radix = do
    char '0'
    r <- oneOf radix
    return ['0', r]

getRadix :: Char -> Int
getRadix r
    | r `elem'` "oO" = 8
    | r `elem'` "dD" = 10
    | r `elem'` "xX" = 16
    | r `elem'` "bB" = 2
    | otherwise = error $ "invalid radix: " ++ [r]
  where
    -- Supriing to me, but GHC fails to infer the types above without this:
    elem' :: Char -> String -> Bool
    elem' = elem

interpIntLit :: String -> Int
interpIntLit lit@('0':r:ds) = case getRadix r of
    8 -> read lit
    16 -> read lit
    10 -> read ds
    2 -> let digits = map (read . (:[])) ds
         in sum $ zipWith shiftL (reverse digits) [0,1..]
interpIntLit lit = read lit


pConstField :: Parser LayoutField
pConstField = token $ do
    width <- pIntLit
    char '\''
    value <- pIntLit
    return (FixedL width value)


-- | Parse a whole source file
pFile = many whitespaceOrComment *> (File <$> many pDecl) <* eof


pDecl :: Parser Decl
pDecl = try pTypeDecl <|> pLayoutDecl

pTypeDecl :: Parser Decl
pTypeDecl = keyword "type" >> TypeDecl <$> pIdent <*> pType

pType :: Parser Type
pType = choice (map try [ pUIntType
                        , pStructType
                        , keyword "bool" >> return BoolT
                        ])

pStructType = do
    keyword "struct" >> keyword "{"
    fields <- many1 $ do
        names <- pIdent `sepBy` token (char ',')
        keyword ":"
        ty <- pType
        return (names, ty)
    keyword "}"
    return $ StructT fields

pUIntType = do
    keyword "uint" >> keyword "<"
    num <- pIntLit
    keyword ">"
    return $ UIntT num

pLayoutDecl :: Parser Decl
pLayoutDecl = do
    keyword "layout"
    params <- option [] pAnnotationList
    name <- pIdent
    keyword "{"
    specs <- many pLayoutSpec
    keyword "}"
    return (LayoutDecl name params specs)

pAnnotationList :: Parser [LayoutParam]
pAnnotationList = do
    keyword "("
    ret <- many (pAnnotation <* keyword ",")
    last <- option [] ((:[]) <$> pAnnotation)
    keyword ")"
    return $ ret ++ last

pAnnotation :: Parser LayoutParam
pAnnotation = choice
    [ keyword "little" >> return (Endian Little)
    , keyword "big" >> return (Endian Big)
    , Align <$> (keyword "align" >> keyword "=" >> pIntLit)
    ]


pLayoutSpec = LayoutSpec [] <$> pLayoutField


pLayoutField :: Parser LayoutField
pLayoutField = pConstField <|> pNamedField

pNamedField = do
    name <- pIdent
    mod <- choice (map try [ pLayoutSlice
                           , pLayoutStruct
                           , return WholeL
                           ])
    return (mod name)

pLayoutSlice = do
    keyword "["
    index1 <- pIntLit
    keyword ":"
    index2 <- pIntLit
    keyword "]"
    return (\name -> SliceL name index1 index2)

pLayoutStruct = pzero
