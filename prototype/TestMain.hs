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
        [ ( unlines
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
              [ TypeDecl "Foo" [] $ StructT
                  [ (["f","g"], UIntT 2)
                  , (["hello"], NamedT "bool" [])
                  ]
              , TypeDecl "Bar" [] $ StructT
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
        , ( unlines
              [ "type GDTEnt struct {"
              , "    base: uint<32>"
              , "    limit: uint<20>"
              , ""
              , "    flags: struct {"
              , "       gr, sz: bool"
              , "    }"
              , ""
              , "    access: struct {"
              , "        ac, rw, dc, ex, pr: bool"
              , "        privl: uint<2>"
              , "    }"
              , "}"
              , ""
              , "layout GDTEnt (little) {"
              , "    limit[15:0]"
              , "    base[23:0]"
              , "    access {"
              , "        ac rw dc ex"
              , "        1'0b1"
              , "        privl"
              , "        pr"
              , "    }"
              , ""
              , "    limit[19:16]"
              , "    flags {"
              , "        2'0b0"
              , "        sz"
              , "        gr"
              , "    }"
              , "    base[24:32]"
              , "}"
              ]
          , Right $ File
              [ TypeDecl "GDTEnt" [] $ StructT
                  [ (["base"], UIntT 32)
                  , (["limit"], UIntT 20)
                  , (["flags"], StructT
                      [ (["gr", "sz"], NamedT "bool" [])
                      ])
                  , (["access"], StructT
                      [ (["ac", "rw", "dc", "ex", "pr"], NamedT "bool" [])
                      , (["privl"], UIntT 2)
                      ])
                  ]
              , LayoutDecl "GDTEnt" [Endian Little]
                  [ LayoutSpec [] (SliceL "limit" 15 0)
                  , LayoutSpec [] (SliceL "base"  23 0)
                  , LayoutSpec [] $ StructL "access"
                      [ LayoutSpec [] (WholeL "ac")
                      , LayoutSpec [] (WholeL "rw")
                      , LayoutSpec [] (WholeL "dc")
                      , LayoutSpec [] (WholeL "ex")
                      , LayoutSpec [] (FixedL 1 1)
                      , LayoutSpec [] (WholeL "privl")
                      , LayoutSpec [] (WholeL "pr")
                      ]
                  , LayoutSpec [] (SliceL "limit" 19 16)
                  , LayoutSpec [] $ StructL "flags"
                      [ LayoutSpec [] (FixedL 2 0)
                      , LayoutSpec [] (WholeL "sz")
                      , LayoutSpec [] (WholeL "gr")
                      ]
                  , LayoutSpec [] (SliceL "base" 24 32)
                  ]
              ]
            )
        ]
    ]
 where parseCompare (text, ast) = TestCase $ assertEqual
        ("parseCompare " ++ show (text, ast))
        (runParser pFile () "test-input" text)
        ast
