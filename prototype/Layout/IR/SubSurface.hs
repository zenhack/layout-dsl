{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances,
    KindSignatures, ConstraintKinds #-}
{-|

TODO: this module is deprecated, and most of the code is commented out; I've
parameterized the Ast so a separate set of types isn't necessary, but still need
to make sure I've ported all of the logic over.

First intermediate form after the Ast. This is semantically the same as a
*legal* Ast, but is capable of representing fewer invalid inputs. We convert
to this as part of validating our input.

This module is called "SubSurface" because (1) it sits just below the surface
syntax, and (2) I(zenhack) can't think of something better right now (TODO:
fix that).
-}
module Layout.IR.SubSurface where
{-

import GHC.Exts (Constraint)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Layout.Ast (ByteOrder(..), TypeParam(..))
import Layout.Validate (SymbolTable)
import qualified Layout.Ast as Ast

data File align byteOrder slice
    = File (M.Map Text (Type align byteOrder slice))

data Type align byteOrder slice
    = Type [Text] Ast.Type (LayoutSpec align byteOrder slice)

-- LayoutSpec and LayoutField are parametrized over a few things,
-- each of kind (* -> *). These are containers for various information
-- that may have different requirements at different strages of
-- translation. In earlier stages these things may be implicit or
-- inherited, whereas in later stages they must be explicit for every
-- field.
data LayoutSpec align byteOrder slice = LayoutSpec
    { alignment :: align Int
    , layoutByteOrder :: byteOrder ByteOrder
    , field :: LayoutField align byteOrder slice
    }

data LayoutField align byteOrder slice
    = SliceL Text Int Int
    | FixedL (slice (Int, Int))
    | StructL Text [LayoutSpec align byteOrder slice]

{-
-- WIP; commented out to keep from breaking CI.

fileFromAst :: SymbolTable -> File Maybe Maybe Maybe
fileFromAst syms = M.map
    (\(Ast.TypeDecl params def, lD) -> Type params def (layoutSpecFromAstDecl lD))
    syms

layoutSpecFromAstDecl :: SymbolTable -> Ast.LayoutDecl
    -> Either [ValidationError] (LayoutSpec Maybe Maybe Maybe)
layoutSpecFromAstDecl (Ast.LayoutDecl params specs) =
    (align, byteOrder) <- parseLayoutParams params
    LayoutSpec
        { alignment = align
        , layoutByteOrder = byteOrder
        , field = StructL $ map .. specs
        }

parseLayoutParams :: [Ast.LayoutParam] -> Either [ValidationError] (Maybe Int, Maybe ByteOrder)
parseLayoutParams = parseLayoutParmas' (Nothing, Nothing) where
    parseLayoutParams' (Just align, _) (Ast.Align align':_) =
        Left [ConflictingLayoutParams [Ast.Align align, Ast.Align align']
    parseLayoutParams' (_, Just byteOrder) (Ast.Endian byteOrder':_) =
        Left [ConflictingLayoutParams [Ast.Endian byteOrder, Ast.Endian byteOrder']
    parseLayoutParams' (align, byteOrder) (Ast.Align n:params) =
        parseLayoutParams' (Just n, byteOrder) params
    parseLayoutParams' (align, byteOrder) (Ast.Endian byteOrder') =
        parseLayoutParams (align, Just byteOrder')
    parseLayoutParams' ret [] = ret
-}
-}
