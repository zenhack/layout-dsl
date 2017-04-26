{-# LANGUAGE RecordWildCards #-}
{-|

Intermediate form that works at the level of close-to-the-machine operations:
Shifts, bitwise logical operators, integer upcasts/downcasts...

These should be available in most any programming language, so this is a good
last leg before generating getters/setters for a target language.
-}
module Language.LDSL.IR.MachineOps where

import Language.LDSL.IR.RangeMap(RangeMap(..), Index(..))
import Data.Word

data ByteN = ByteN Int
data Arg = Arg

data Expr src
    = Src src
    | BinOp BinOp (Expr src) (Expr src)
    | BitNot (Expr src)
    | Cast Int (Expr src)
    | Const Word64

data BinOp
    = BitAnd
    | BitOr
    | ShiftL
    | ShiftR

data GetStmt = GetStmt (Expr ByteN)
data SetStmt = SetStmt Word64 (Expr Arg)

data Field = Field
    { fieldGetter :: GetStmt
    , fieldSetter :: [SetStmt]
    }

getFieldOps :: [(RangeMap Index, Int)] -> Field
getFieldOps rm = Field
    { fieldGetter = getFieldRange rm
    , fieldSetter = setFieldRange (map fst rm)
    }

getFieldRange :: [(RangeMap Index, Int)] -> GetStmt
getFieldRange [] = GetStmt $ Const 0 -- TODO: not sure this should ever occur
getFieldRange ranges = GetStmt $
    foldl
        (BinOp BitOr)
        (Const 0)
        $ map (uncurry getByteRange) ranges

getByteRange :: RangeMap Index -> Int -> Expr ByteN
getByteRange RangeMap{..} sz =
    let
        Index index = rangeInfo
        upcast      = Cast sz $ Src $ ByteN index
        shifted     = BinOp ShiftR upcast (Const $ fromIntegral rangeLayoutOff)
        mask        = fromIntegral $ 2 ^ rangeLen - 1
        truncated   = BinOp BitAnd (Const mask) shifted
        final       = BinOp ShiftL truncated (Const $ fromIntegral rangeTypeOff)
    in
        final


setFieldRange :: [RangeMap Index] -> [SetStmt]
setFieldRange = map setByteRange

setByteRange :: RangeMap Index -> SetStmt
setByteRange RangeMap{..} =
    let
        Index index = rangeInfo
        mask        = fromIntegral $ 2 ^ rangeLen - 1
        shifted     = BinOp ShiftR (Src Arg) (Const $ fromIntegral rangeLayoutOff)
        result      = BinOp BitAnd (Const mask) shifted
    in
        SetStmt (fromIntegral index) result
