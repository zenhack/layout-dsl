module Main where

import Test.Framework (defaultMain)
import Tests.Parser (parseTests)
import Tests.Validate (buildSymsTests, parseLayoutParamsTests)
import Tests.IR.RangeMap (rangeMapTests)

-- Just so we're building these at all; will probably remove once we have
-- actual tests.
import qualified Layout.IR.MachineOps
import qualified Layout.IR.SubSurface
import qualified Layout.Target.C
import qualified Layout.Target.Haskell

main :: IO ()
main = defaultMain
    [ parseTests
    , rangeMapTests
    , buildSymsTests
    , parseLayoutParamsTests
    ]
