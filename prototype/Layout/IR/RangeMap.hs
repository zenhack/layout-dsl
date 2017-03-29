{-# LANGUAGE UndecidableInstances, RecordWildCards #-}
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
--
-- This also removes explicit endianness information; when it is done all
-- range maps go from low addresses to high.
--
-- XXX TODO: having both of the above in one step is rather awkward; can we
-- tease these apart?
chunkBytes :: FieldMap ByteOrder -> FieldMap Index
chunkBytes (FieldMap fm) = FieldMap (map chunkField fm)
  where
    chunkField (name, ranges) = (name, concat $ map chunkRange ranges)
    chunkRange rm@RangeMap{..} = case rangeInfo of
        -- all of our indexing is from low addresses to high, so if we're
        -- looking at a big-endian range, it will be backwards.
        Little -> chunkRange' rm
        Big -> flipEndianBytes (chunkRange' rm)
    chunkRange' rm =
        let
            startByte = rangeLayoutOff rm `div` 8
            startBit = rangeLayoutOff rm `mod` 8
        in
        if startBit + rangeLen rm >= 8 then
            let lenFst = 8 - startBit
            in (rm { rangeLen = lenFst
                   , rangeLayoutOff = startBit
                   , rangeInfo = Index startByte
                   })
               : chunkRange'
                     (RangeMap
                        { rangeLen = rangeLen rm - lenFst
                        , rangeTypeOff = rangeTypeOff rm + lenFst
                        , rangeLayoutOff = rangeLayoutOff rm + lenFst
                        , rangeInfo = rangeInfo rm
                        })
        else
            [(rm { rangeLayoutOff = startBit
                 , rangeInfo = Index startByte
                 })]
    -- takes a list of chunked rangemaps (output from chunkRange'), and "flips"
    -- them. i.e. The mapping of bits will mirror the input. This is used to
    -- transform little endian mappings to big endian mappings.
    flipEndianBytes xs
        | length xs < 2 = xs
        | otherwise =
            let
                (begin:rest) = xs
                (end:mid) = reverse rest
            in
                reverseInfo $ [flipEndianBits end] ++ mid ++ [flipEndianBits begin]
    -- like flipEndianBits, but for a single sub-byte rangemap.
    flipEndianBits rm@RangeMap{..} =
        let
            startByte = rangeLayoutOff `div` 8
            oldStartBit = rangeLayoutOff `mod` 8
            endBit = 7 - oldStartBit
            newStartBit = endBit + rangeLen
        in
            rm { rangeLayoutOff = startByte + newStartBit }
    -- | takes a list of Rangmaps, and flips the order of their infos, i.e.
    -- @rangeInfo rms !! i == (reverseInfo rms !! length rms - 1 - i)@
    reverseInfo rms =
        let
            infos = map rangeInfo rms
            setInfo rm info = rm { rangeInfo = info }
        in
        zipWith setInfo rms (reverse infos)


{- TODO:

-- | FromAst computes field maps for each of the data types defined in the
-- symbol table.
fromAst :: Validated Ast.SymbolTable -> [(Text, FieldMap Identity)]
-}
