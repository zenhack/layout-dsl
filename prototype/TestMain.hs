module Main where

import Test.HUnit (assertEqual, Test(TestCase, TestList))
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Layout.Ast
import Layout.Parser (pFile)
import Text.ParserCombinators.Parsec (runParser)

main :: IO ()
main = defaultMain
    [ testGroup "Parse Tests" $ hUnitTestToTests $ TestList $ map parseCompare
        [
            ( unlines
                [ "type Foo struct {"
                , "    f, g: uint<2>"
                , "    hello: bool"
                , "}"
                , ""
                , "type Bar struct {"
                , "    substruct: struct {"
                , "        w, x, y, z: uint<64>"
                , "    }"
                , "}"
                , ""
                , "layout Bar {"
                , "    substruct"
                , "    2'0b11"
                , "    foo[0:0xff]"
                , "}"
                ]
            , Right $ File
                [ TypeDecl "Foo" $ StructT
                    [ (["f","g"], UIntT 2)
                    , (["hello"], BoolT)
                    ]
                , TypeDecl "Bar" $ StructT
                    [ (["substruct"], StructT
                         [ (["w","x","y","z"],UIntT 64)
                         ])
                    ]
                , LayoutDecl "Bar" []
                    [ LayoutSpec [] (WholeL "substruct")
                    , LayoutSpec [] (FixedL 2 3)
                    , LayoutSpec [] (SliceL "foo" 0 255)
                    ]
                ]
            )
        ]
    ]
 where parseCompare (text, ast) = TestCase $ assertEqual
        ("parseCompare " ++ show (text, ast))
        (runParser pFile () "test-input" text)
        ast
