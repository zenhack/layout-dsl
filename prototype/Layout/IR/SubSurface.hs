{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
{-|

First intermediate form after the Ast. This is semantically the same as a
*legal* Ast, but is capable of representing fewer invalid inputs. We convert
to this as part of validating our input.

This module is called "SubSurface" because (1) it sits just below the surface
syntax, and (2) I(zenhack) can't think of something better right now (TODO:
fix that).
-}
module Layout.IR.SubSurface where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Layout.Ast (ByteOrder(..), TypeParam(..))
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


-- Derview Show and Eq for all of the above types, given
deriving instance (Show (Type align byteOrder slice)) =>
    Show (File align byteOrder slice)
deriving instance (Show (LayoutSpec align byteOrder slice)) =>
    Show (Type align byteOrder slice)
deriving instance (Show (align Int), Show (byteOrder ByteOrder), Show (slice (Int, Int))) =>
    Show (LayoutSpec align byteOrder slice)
deriving instance (Show (align Int), Show (byteOrder ByteOrder), Show (slice (Int, Int))) =>
    Show (LayoutField align byteOrder slice)

deriving instance (Eq (Type align byteOrder slice)) =>
    Eq (File align byteOrder slice)
deriving instance (Eq (LayoutSpec align byteOrder slice)) =>
    Eq (Type align byteOrder slice)
deriving instance (Eq (align Int), Eq (byteOrder ByteOrder), Eq (slice (Int, Int))) =>
    Eq (LayoutSpec align byteOrder slice)
deriving instance (Eq (align Int), Eq (byteOrder ByteOrder), Eq (slice (Int, Int))) =>
    Eq (LayoutField align byteOrder slice)
