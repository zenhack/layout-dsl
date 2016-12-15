{-|
Backend in the form of an interpreter; converts intermediate forms to haskell
helper functions at run-time.
-}
module Layout.Target.Haskell (interpGet, interpSet) where

import Data.Bits
import Data.Word
import Layout.IR.MachineOps


interpByteN :: B.ByteString -> ByteN -> Word64
interpByteN bs (ByteN n) = fromIntegral $ B.index bs n

interpArg :: Word64 -> Arg -> Word64
interpArg n _ = n

interpExpr :: (src -> Word64) -> Expr src -> Word64
interpExpr interpSrc (Src s) = interpSrc s
interpExpr interpSrc (BinOp op l r) =
    (interpBinOp op)
        (interpExpr interpSrc l)
        (interpExpr interpSrc r)
interpExpr interpSrc (BitNot expr) = complement (interpExpr interpSrc expr)
-- We keep everything in Word64 throughout, because it makes an interpreter's
-- types awkward otherwise. But we still need to truncate things when we process
-- a downcast:
interpExpr interpSrc (Cast n expr) = (interpExpr interpSrc expr) `mod` (1 `shiftL` n)
interpExpr _ (Const n) = n

interpBinOp BitAnd = (.&.)
interpBinOp BitOr  = (.|.)
interpBinOp ShiftL = shiftL
interpBinOp ShiftR = shiftR

interpGet :: B.ByteString -> Expr ByteN -> Word64
interpGet bs = interpExpr (interpByteN bs)

interpSet :: Word64 -> Expr Arg -> Word64
interpSet arg = interpExpr (interpArg arg)
