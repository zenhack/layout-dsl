--module Layout.Parser (pFile) where
module Layout.Parser where

-- General note: we make use of fewer of parsec's features and pre-existing
-- helpers than might otherwise sense, favoring instead having this
-- implementation closely follow the grammar in ../grammar.md. This should make
-- it easier to verify that the implementation is correct.
--
-- Parts of the lexer are named l*, while parts of the parser are named p*

-- These are in Prelude on more recent versions of base, but not older ones; we
-- may as well import them:
import Control.Applicative ((<$>), (<*), (<*>), (*>))

import Control.Monad (void, when)
import Data.Bits(shiftL)
import Data.Char (toLower)
import Text.ParserCombinators.Parsec hiding(token)
import qualified Data.Text as T
import Data.Text (Text)
import Layout.Ast


lLetter, lBinaryDigit, lDecimalDigit, lOctalDigit, lHexDigit :: Parser Char

lLetter = oneOf $ ['A'..'Z'] ++ ['a'..'z'] ++ ['_']
lBinaryDigit = oneOf "01"
lDecimalDigit = oneOf ['0'..'9']
lOctalDigit = oneOf ['0'..'7']
lHexDigit = oneOf $ ['0'..'9'] ++ ['A'..'F'] ++ ['a'..'f']

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

-- | @commaList p@ parses a list of p separated by commas, and optionally
--   ending with a comma.
commaList :: Parser a -> Parser [a]
commaList p = do
    first <- many $ try (p <* keyword ",")
    last <- option [] ((:[]) <$> try p)
    return $ first ++ last

-- | @token p@ parses @p@ with optional trailing whitespace/comments.
token :: Parser a -> Parser a
token p = try (p <* many whitespaceOrComment)

-- | @keyword p@ parses the literal token @p@.
keyword :: String -> Parser ()
keyword = void . token . string

-- | list of reserved words that would otherwise be legal identifiers
reservedWords =
    [ "layout"
    , "struct"
    , "type"
    , "uint"
    ]


-- Parse an identifier. This fails if the token is a reserved word.
pIdent = token (do
            name <- pMaybeIdentifier
            when (name `elem` reservedWords) pzero
            return $ T.pack name)
    <?> "identifier"
  where
    -- Parse something which *may* be an identifier, if it is not a reserved word.
    pMaybeIdentifier = ((:) <$> lLetter <*> many (lLetter <|> lDecimalDigit))


lIntLit            = choice $ map try [ lDecimalLit
                                      , lOctalLit
                                      , lHexLit
                                      , lBinaryLit
                                      , string "0" >> return 0
                                      ]
lDecimalLit        = lNoradixDecimalLit <|> lRadixDecimalLit
lNoradixDecimalLit = read <$> ((:) <$> oneOf ['1'..'9'] <*> many lDecimalDigit)
lRadixDecimalLit   = char '0' >> oneOf "dD"  >> (read <$> many1 lDecimalDigit)
lOctalLit          = read <$> (cons2 <$> char '0'
                                     <*> option 'o' (oneOf "oO")
                                     <*> many1 lOctalDigit)
lHexLit            = read <$> (cons2 <$> char '0'
                                     <*> oneOf "xX"
                                     <*> many1 lHexDigit)
lBinaryLit         = char '0' >> oneOf "bB"  >> (readBin <$> many1 lBinaryDigit)

cons2 x y zs = x:y:zs

readBin ds = sum $ zipWith shiftL (reverse digits) [0,1..]
  where
    digits = map interpBit ds
    interpBit b = case b of
        '0' -> 0
        '1' -> 1

pIntLit = token lIntLit

pConstField :: Parser LayoutField
pConstField = token $ do
    width <- lIntLit
    char '\''
    value <- lIntLit
    return $ ConstLF $ FixedL width value


pType :: Parser Type
pType = (NamedT <$> pIdent <*> option [] pTypeParamList) <|> pTypeLit
pTypeParam = choice
    [ TypeVar <$> pIdent
    , TypeNum <$> pIntLit
    ]
pTypeParamList = keyword "<" *> commaList pTypeParam <* keyword ">"
pTypeLit = pStructType <|> pUIntType
pStructType = do
    keyword "struct" >> keyword "{"
    fields <- many ((,) <$> (pIdentList <* keyword ":") <*> pType)
    keyword "}"
    return $ StructT fields
pUIntType = UIntT <$> (keyword "uint" >> keyword "<" >> pIntLit <* keyword ">")

pIdentList = commaList pIdent

pLayout :: Parser LayoutSpec
pLayout = LayoutSpec <$> option [] pAnnotationList
                     <*> pLayoutField

pLayoutField :: Parser LayoutField
pLayoutField =
    pConstField <|> pNamedField

pNamedField :: Parser LayoutField
pNamedField = NamedLF <$> pIdent <*> choice
    [ (uncurry SliceL) <$> pLayoutSlice
    , StructL <$> (keyword "{" *> many pLayout <* keyword "}")
    , return $ WholeL
    ]
pLayoutSlice = do
    keyword "["
    l <- pIntLit
    keyword ":"
    r <- pIntLit
    keyword "]"
    return (l, r)

pAnnotation :: Parser LayoutParam
pAnnotation = choice
    [ keyword "little" >> return (Endian Little)
    , keyword "big" >> return (Endian Big)
    , Align <$> (keyword "align" >> keyword "=" >> pIntLit)
    ]

pAnnotationList :: Parser [LayoutParam]
pAnnotationList = keyword "(" *> commaList pAnnotation <* keyword ")"

-- | Parse a whole source file
pFile = many whitespaceOrComment *> (File <$> many pDecl) <* eof


pDecl :: Parser (Text, Decl)
pDecl = pTypeDecl <|> pLayoutDecl

pTypeDecl :: Parser (Text, Decl)
pTypeDecl = do
    keyword "type"
    name <- pIdent
    params <- option [] (keyword "<" *> many pIdent <* keyword ">")
    ty <- pType
    return (name, TypeD $ TypeDecl params ty)

pLayoutDecl :: Parser (Text, Decl)
pLayoutDecl = do
    keyword "layout"
    name <- pIdent
    params <- option [] pAnnotationList
    keyword "{"
    specs <- many pLayout
    keyword "}"
    return (name, LayoutD $ LayoutDecl params specs)
