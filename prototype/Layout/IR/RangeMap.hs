{-# LANGUAGE UndecidableInstances #-}
{-|

Intermediate form mapping bit ranges in the logical structure to bit ranges
in the physical structure.

There are conceptually two phases of translation handled here. Both deal
with mappings of field names to pairs of locations, one in the logical layout
and one in the physical.

In the first stage, these are just the flat bit ranges, relative to the start
of the whole cases. In the second case, we split these into ranges with a
maximum length of 8 bits, and tag them with the byte index from the start of
the struct.

The structures of these two intermediate forms are very similar, so 'FieldMap'
handles both of these. 'FieldMap' has kind '(* -> *) -> *', where the parameter
is (for our purposes) either Idenity or Indexed. In the first stage it is
'Identity', which adds no extra information. In the second, it is 'Indexed',
which adds the byte index.

'chunkBytes' converts from the first stage to the second.
-}
module Layout.IR.RangeMap where

import Control.Monad.Identity (Identity(Identity))
import Data.Text (Text)

data Indexed a = Indexed Int a deriving(Show, Eq)

data RangeMap = RangeMap
    { rangeLen :: Int -- ^ the length of the range
    , rangeTypeOff :: Int -- ^ the offset into  the type's field.
    , rangeLayoutOff :: Int -- ^ the offset into the layout.
    } deriving(Show, Eq)

data FieldMap bucket
    = FieldMap [(Text, [bucket RangeMap])]

instance Show (bucket RangeMap) => Show (FieldMap bucket) where
    show (FieldMap fm) = "FieldMap " ++ show fm
instance Eq (bucket RangeMap) => Eq (FieldMap bucket) where
    (==) (FieldMap fm) (FieldMap fm') = fm == fm'


-- @chunkBytes@ transforms its argument to one where all @RangeMap@s fit neatly
-- inside of one byte within the physical structure. See the module-level
-- comment for further discussion.
chunkBytes :: FieldMap Identity -> FieldMap Indexed
chunkBytes (FieldMap fm) = FieldMap (map chunkField fm)
  where
    chunkField (name, ranges) = (name, concat $ map chunkRange ranges)
    chunkRange (Identity rm) =
        let
            startByte = rangeLayoutOff rm `div` 8
            startBit = rangeLayoutOff rm `mod` 8
        in
        if startBit + rangeLen rm >= 8 then
            let lenFst = 8 - startBit
            in Indexed startByte (rm { rangeLen = lenFst
                                     , rangeLayoutOff = startBit
                                     })
               : chunkRange
                     (Identity RangeMap
                        { rangeLen = rangeLen rm - lenFst
                        , rangeTypeOff = rangeTypeOff rm + lenFst
                        , rangeLayoutOff = rangeLayoutOff rm + lenFst
                        })
        else
            [Indexed startByte (rm { rangeLayoutOff = startBit})]
