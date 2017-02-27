{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances,
    ConstraintKinds #-}
{-|

Abstract synatx tree for the DSL.
-}
module Layout.Ast where

import GHC.Exts (Constraint)
import Data.Text (Text)
import qualified Data.Map as M
import Data.Functor.Identity (Identity)

-- The Ast is parametrized over a few type (operators) so that the types can be
-- strengthened as the translation goes on. We define a few type synonyms for
-- specific stages in the translation:
--
-- * ParseStage is a helper for specifiying the monomorphized types that
--   come out of the parser.
-- * Validated specifies the types once the validation step is complete.
type ParseStage t = t [LayoutParam] Maybe
type Validated t = t (Maybe Int, Maybe ByteOrder) Identity

-- | A symbol table, mapping names to type, layout pairs.
newtype SymbolTable lParams slice
    = SymbolTable (M.Map Text (TypeDecl, LayoutDecl lParams slice))

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
