module Tests.Parser where

import Test.HUnit (assertEqual, Test(TestCase, TestList))
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Layout.Ast
import Layout.Parser (pFile)
import Text.ParserCombinators.Parsec (runParser)


sliceL l r = SliceL (Just (l, r))
wholeL = SliceL Nothing

parseTests = testGroup "Parse Tests" $ hUnitTestToTests $ TestList $ map parseCompare
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
          [ ("Foo", TypeD $ TypeDecl [] $ StructT
              [ (["f","g"], UIntT 2)
              , (["hello"], NamedT "bool" [])
              ])
          , ("Bar", TypeD $ TypeDecl [] $ StructT
              [ (["substruct"], StructT
                   [ (["w","x","y","z"],UIntT 64)
                   ])
              ])
          , ("Bar", LayoutD $ LayoutDecl []
              [ LayoutSpec [] (NamedLF "substruct" wholeL)
              , LayoutSpec [] (ConstLF $ FixedL 2 3)
              , LayoutSpec [] (NamedLF "foo" $ sliceL 0 255)
              ])
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
          [ ("GDTEnt", TypeD $ TypeDecl [] $ StructT
              [ (["base"], UIntT 32)
              , (["limit"], UIntT 20)
              , (["flags"], StructT
                  [ (["gr", "sz"], NamedT "bool" [])
                  ])
              , (["access"], StructT
                  [ (["ac", "rw", "dc", "ex", "pr"], NamedT "bool" [])
                  , (["privl"], UIntT 2)
                  ])
              ])
          , ("GDTEnt", LayoutD $ LayoutDecl [Endian Little]
              [ LayoutSpec [] (NamedLF "limit" $ sliceL 15 0)
              , LayoutSpec [] (NamedLF "base" $ sliceL 23 0)
              , LayoutSpec [] $ NamedLF "access" $ StructL
                  [ LayoutSpec [] $ NamedLF "ac" wholeL
                  , LayoutSpec [] $ NamedLF "rw" wholeL
                  , LayoutSpec [] $ NamedLF "dc" wholeL
                  , LayoutSpec [] $ NamedLF "ex" wholeL
                  , LayoutSpec [] $ ConstLF $ FixedL 1 1
                  , LayoutSpec [] $ NamedLF "privl" wholeL
                  , LayoutSpec [] $ NamedLF "pr" wholeL
                  ]
              , LayoutSpec [] $ NamedLF "limit" $ sliceL 19 16
              , LayoutSpec [] $ NamedLF "flags" $ StructL
                  [ LayoutSpec [] $ ConstLF $ FixedL 2 0
                  , LayoutSpec [] $ NamedLF "sz" wholeL
                  , LayoutSpec [] $ NamedLF "gr" wholeL
                  ]
              , LayoutSpec [] $ NamedLF "base" $ sliceL 24 32
              ])
          ]
        )
    ]
 where parseCompare (text, ast) = TestCase $ assertEqual
        ("parseCompare " ++ show (text, ast))
        (runParser pFile () "test-input" text)
        ast
