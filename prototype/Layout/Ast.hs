{-|

Abstract synatx tree for the DSL.
-}
module Layout.Ast where

import Data.Text(Text)

data File = File [Decl] deriving(Show, Eq)

-- | A top-level declaration
data Decl
    -- logical layout declaration
    = TypeDecl
        Text -- ^ type name
        [Text] -- ^ type parameter names
        Type -- ^ definition
    -- | Physical layout declaration
    | LayoutDecl
        Text -- ^ type name
        [LayoutParam]
        [LayoutSpec]
    deriving(Show, Eq)

data Type
    = StructT
        [([Text], Type)] -- ^ (fieldname, type) pairs
    | UIntT
        Int -- ^ bit width
    | NamedT Text [TypeParam]
    deriving(Show, Eq)

data TypeParam
    = TypeVar Text
    | TypeNum Int
    deriving(Show, Eq)

data LayoutParam
    = Endian ByteOrder
    | Align Int
    deriving(Show, Eq)

data ByteOrder = Little | Big deriving(Show, Eq)

data LayoutSpec = LayoutSpec [LayoutParam] LayoutField deriving(Show, Eq)

data LayoutField
    = SliceL
        Text -- ^ name
        Int -- ^ first index
        Int -- ^ second index
    | FixedL
        Int -- ^ length
        Int -- ^ value
    | StructL
        Text -- ^ name
        [LayoutSpec] -- ^ fields
    | WholeL
        Text -- ^ field name
    deriving(Show, Eq)
