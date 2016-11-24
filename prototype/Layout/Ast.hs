{-|

Abstract synatx tree for the DSL.
-}
module Layout.Ast where

import Data.Text(Text)

data File = File [Decl] deriving(Show)

-- | A top-level declaration
data Decl
    -- logical layout declaration
    = TypeDecl
        Text -- ^ type name
        Type -- ^ definition
    -- | Physical layout declaration
    | LayoutDecl
        Text -- ^ type name
        [LayoutParam]
        [LayoutSpec]
    deriving(Show)

data Type
    = StructT
        [(Text, Type)] -- ^ (fieldname, type) pairs
    | UIntT
        Int -- ^ bit width
    deriving(Show)

data LayoutParam
    = Endian ByteOrder
    | Align Int
    deriving(Show)

data ByteOrder = Little | Big deriving(Show)

data LayoutSpec = LayoutSpec [LayoutParam] LayoutField deriving(Show)

data LayoutField
    = SliceL
        Text -- ^ field name
        Int -- ^ first index
        Int -- ^ second index
    | FixedL
        Int -- ^ length
        Int -- ^ radix
        Int -- ^ value
    | StructL
        [(Text, LayoutSpec)] -- ^ (fieldname, type) pairs
    deriving(Show)
