module Layout.Ast where

import Data.Text(Text)

data Ast = Ast [Decl] deriving(Show)

data Decl
    = TypeDecl Text Type
    | LayoutDecl Text [LayoutParam] [LayoutSpec]
    deriving(Show)

data Type
    = StructT [(Text, Type)]
    | UIntT Int
    deriving(Show)

data LayoutParam
    = Endian ByteOrder
    | Align Int
    deriving(Show)

data ByteOrder = Little | Big deriving(Show)

data LayoutSpec = LayoutSpec [LayoutParam] LayoutField deriving(Show)

data LayoutField
    = SliceL Text Int Int
    | FixedL Int Int Int
    | StructL [(Text, LayoutSpec)]
    deriving(Show)
