{-# LANGUAGE UndecidableInstances #-}
{-|

Intermediate form mapping bit ranges in the logical structure to bit ranges
in the physical structure.

There are a few phases of translation handled here. They deal with mappings
of field names to pairs of locations, one in the logical layout and one in the
physical.

In the first stage, these are just the flat bit ranges, relative to the start
of the whole types & layouts, plus the byte order of the fields.

In the second stage, we split these into ranges with a maximum length of 8
bits, and tag them with the byte index from the start of the struct.

In the final stage, we remove the byte order info, manipulating the offsets
to make everything work without it.

The structures of these intermediate forms are similiar, so the same basic
types ('FieldMap' and 'RangeMap') handle each of them. They take an info
type paramter that carries the metadata needed for each stage -- the input
to the first stage has @info = ByteOrder@, the next has @info = (Index,
ByteOrder)@, and the final has @info = Index@.

'chunkBytes' converts from the first stage to the second. The second-to-third
stage translation is not yet implemented.
-}
module Layout.IR.RangeMap where

import Control.Monad.Identity (Identity(Identity))
import Data.Text (Text)
import Layout.Ast (ByteOrder(..))

newtype Index = Index Int deriving(Show, Eq)

data RangeMap info = RangeMap
    { rangeLen :: Int -- ^ the length of the range
    , rangeTypeOff :: Int -- ^ the offset into  the type's field.
    , rangeLayoutOff :: Int -- ^ the offset into the layout.
    , rangeInfo :: info
    } deriving(Show, Eq)

data FieldMap info
    = FieldMap [(Text, [RangeMap info])]
    deriving(Show, Eq)

{-
instance Show (bucket RangeMap) => Show (FieldMap bucket) where
    show (FieldMap fm) = "FieldMap " ++ show fm
instance Eq (bucket RangeMap) => Eq (FieldMap bucket) where
    (==) (FieldMap fm) (FieldMap fm') = fm == fm'
-}


-- @chunkBytes@ transforms its argument to one where all @RangeMap@s fit neatly
-- inside of one byte within the physical structure. See the module-level
-- comment for further discussion.
chunkBytes :: FieldMap ByteOrder -> FieldMap (Index, ByteOrder)
chunkBytes (FieldMap fm) = FieldMap (map chunkField fm)
  where
    chunkField (name, ranges) = (name, concat $ map chunkRange ranges)
    chunkRange rm =
        let
            startByte = rangeLayoutOff rm `div` 8
            startBit = rangeLayoutOff rm `mod` 8
        in
        if startBit + rangeLen rm >= 8 then
            let lenFst = 8 - startBit
            in (rm { rangeLen = lenFst
                   , rangeLayoutOff = startBit
                   , rangeInfo = (Index startByte, rangeInfo rm)
                   })
               : chunkRange
                     (RangeMap
                        { rangeLen = rangeLen rm - lenFst
                        , rangeTypeOff = rangeTypeOff rm + lenFst
                        , rangeLayoutOff = rangeLayoutOff rm + lenFst
                        , rangeInfo = rangeInfo rm
                        })
        else
            [(rm { rangeLayoutOff = startBit
                 , rangeInfo = (Index startByte, rangeInfo rm)
                 })]


{- TODO:

-- | FromAst computes field maps for each of the data types defined in the
-- symbol table.
fromAst :: Validated Ast.SymbolTable -> [(Text, FieldMap Identity)]
-}
