{-|

Intermediate form that works at the level of close-to-the-machine operations:
Shifts, bitwise logical operators, integer upcasts/downcasts...

These should be available in most any programming language, so this is a good
last leg before generating getters/setters for a target language.
-}
module Layout.IR.MachineOps where

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
