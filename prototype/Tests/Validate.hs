module Tests.Validate where

import Layout.Ast
import Layout.Validate
import qualified Data.Map.Strict as M
import Test.HUnit (assertEqual, Test(TestCase, TestList))
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)


buildSymsTests = testGroup "buildSyms Tests" $ hUnitTestToTests $ TestList $
    map buildSymsCompare
      [ ( File []
        , Right $ SymbolTable M.empty
        )
      , ( File
          [ ("Foo", TypeD $ TypeDecl [] $ StructT [])
          , ("Foo", LayoutD $ LayoutDecl [] [])
          ]
        , Right $ SymbolTable $M.fromList
                [("Foo",
                  ( TypeDecl [] $ StructT []
                  , LayoutDecl [] []
                  ))
                ]
        )
      , ( File [("Foo", TypeD $ TypeDecl [] $ StructT [])]
        , Left [OrphanDecl "Foo" $ TypeD $ TypeDecl [] $ StructT []]
        )
      ]
 where buildSymsCompare (ast@(File decls), syms) = TestCase $ assertEqual
        ("buildSymsCompare " ++ show (ast, syms))
        syms
        (buildSyms decls)


parseLayoutParamsTests = testGroup "parseLayoutParams Tests" $
    hUnitTestToTests $ TestList $ map parseLayoutParamsCompare
        [ ( Left [ConflictingLayoutParams [Align 2, Align 4]]
          , [Align 2, Align 4]
          )
        , ( Right (Just 2, Nothing)
          , [Align 2]
          )
        , ( Right (Nothing, Just Little)
          , [Endian Little]
          )
        , ( Right (Just 4, Just Big)
          , [Endian Big, Align 4]
          )
        ]
 where parseLayoutParamsCompare (results, params) = TestCase $ assertEqual
        ("parseLayoutParamsCompare  " ++ show (results, params))
        results
        (parseLayoutParams params)
