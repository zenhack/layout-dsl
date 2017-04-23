{-|

Backend which generates C code
-}
module Language.LDSL.Target.C (interpGet, interpSet) where

import Language.LDSL.IR.MachineOps
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

interpByteN :: Text -> ByteN -> Text
interpByteN bytesName (ByteN n) = bytesName <> "[" <> T.pack (show n) <> "]"

interpArg :: Text -> Arg -> Text
interpArg arg _ = arg

interpExpr :: (src -> Text) -> Expr src -> Text
interpExpr interpSrc (Src src) = interpSrc src
interpExpr interpSrc (BinOp op l r) =
    "(" <> interpExpr interpSrc l <> ") "
    <> interpBinOp op <>
    " (" <> interpExpr interpSrc r <> ")"
interpExpr interpSrc (BitNot expr) = "~(" <> interpExpr interpSrc expr <> ")"
interpExpr interpSrc (Cast n expr) =
    "(uint" <> T.pack (show n) <> "_t)(" <> interpExpr interpSrc expr <> ")"
interpExpr _ (Const n) = T.pack $ show n

interpGet :: Text -> Expr ByteN -> Text
-- ^ @interpGet byteName expr@ is a C source code expression computing the given
-- Expr, assuming the byte array is in scope as the r-value @byteName@
interpGet bytesName = interpExpr (interpByteN bytesName)

interpSet :: Text -> Expr Arg -> Text
interpSet argName = interpExpr (interpArg argName)

interpBinOp BitAnd = "&"
interpBinOp BitOr  = "|"
interpBinOp ShiftL = "<<"
interpBinOp ShiftR = ">>"
