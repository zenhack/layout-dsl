{-|

Abstract synatx tree for the DSL.
-}
module Layout.Ast where

import Data.Text(Text)

data File = File [(Text, Decl)] deriving(Show, Eq)

-- | A top-level declaration
data Decl
    = TypeD TypeDecl
    | LayoutD LayoutDecl
    deriving(Show, Eq)

-- | logical layout declaration
data TypeDecl = TypeDecl
        [Text] -- ^ type parameter names
        Type -- ^ definition
        deriving(Show, Eq)

-- | Physical layout declaration
data LayoutDecl = LayoutDecl [LayoutParam] [LayoutSpec] deriving(Show, Eq)

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
    = NamedLF Text NamedLF
    | ConstLF ConstLF
    deriving(Show, Eq)

data NamedLF
    = SliceL
        Int -- ^ first index
        Int -- ^ second index
    | StructL
        [LayoutSpec] -- ^ fields
    | WholeL
    deriving(Show, Eq)

data ConstLF
    = FixedL
        Int -- ^ length
        Int -- ^ value
    deriving(Show, Eq)
