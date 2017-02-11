{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances,
    ConstraintKinds #-}
{-|

Abstract synatx tree for the DSL.
-}
module Layout.Ast where

import GHC.Exts (Constraint)
import Data.Text (Text)

data File lParams slice
    = File [(Text, Decl lParams slice)]

-- | A top-level declaration
data Decl lParams slice
    = TypeD TypeDecl
    | LayoutD (LayoutDecl lParams slice)

-- | logical layout declaration
data TypeDecl = TypeDecl
        [Text] -- ^ type parameter names
        Type -- ^ definition
        deriving(Show, Eq)

-- | Physical layout declaration
data LayoutDecl lParams slice
    = LayoutDecl lParams [LayoutSpec lParams slice]

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

data LayoutSpec lParams slice
    = LayoutSpec lParams (LayoutField lParams slice)

data LayoutField lParams slice
    = NamedLF Text (NamedLF lParams slice)
    | ConstLF ConstLF

data NamedLF lParams slice
    = SliceL (slice (Int, Int))
    | StructL [LayoutSpec lParams slice]

data ConstLF
    = FixedL
        Int -- ^ length
        Int -- ^ value
    deriving(Show, Eq)


deriving instance ParamCtx Show lParams slice => Show (NamedLF lParams slice)
deriving instance ParamCtx Eq   lParams slice => Eq   (NamedLF lParams slice)
deriving instance ParamCtx Show lParams slice => Show (Decl lParams slice)
deriving instance ParamCtx Eq   lParams slice => Eq   (Decl lParams slice)
deriving instance ParamCtx Show lParams slice => Show (LayoutDecl  lParams slice)
deriving instance ParamCtx Eq   lParams slice => Eq   (LayoutDecl  lParams slice)
deriving instance ParamCtx Show lParams slice => Show (LayoutSpec  lParams slice)
deriving instance ParamCtx Eq   lParams slice => Eq   (LayoutSpec  lParams slice)
deriving instance ParamCtx Show lParams slice => Show (LayoutField lParams slice)
deriving instance ParamCtx Eq   lParams slice => Eq   (LayoutField lParams slice)
deriving instance ParamCtx Show lParams slice => Show (File lParams slice)
deriving instance ParamCtx Eq   lParams slice => Eq   (File lParams slice)
type ParamCtx (cls :: * -> Constraint) lParams slice =
    ( cls lParams
    , cls (slice (Int, Int))
    )
