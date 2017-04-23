{-# LANGUAGE UndecidableInstances #-}
module Tests.IR.RangeMap where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Identity (Identity(..))
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneof)
import Test.QuickCheck.Instances
import Language.LDSL.Ast (ByteOrder(..))
import Language.LDSL.IR.RangeMap
    ( FieldMap(..)
    , Index(..)
    , RangeMap(..)
    , chunkBytes
    )

instance Arbitrary Index where
    arbitrary = Index <$> arbitrary

instance Arbitrary ByteOrder where
    arbitrary = oneof $ map return [Big, Little]

instance Arbitrary info => Arbitrary (RangeMap info) where
    arbitrary = RangeMap <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary info => Arbitrary (FieldMap info) where
    arbitrary = FieldMap <$> arbitrary

totalSize :: FieldMap info -> Int
totalSize = sum . map rangeLen . allRangeMaps

allRangeMaps (FieldMap fm) = concat $ map snd fm

rangeMapTests = testGroup "Range map tests"
    [ testProperty "chunkBytes preserves total size."
        (\fm -> totalSize fm == totalSize (chunkBytes fm))
    , testProperty "chunkBytes doesn't make ranges > 8 bits"
        (\fm -> and $ map ((<= 8) . rangeLen) $ allRangeMaps (chunkBytes fm))
    ]
