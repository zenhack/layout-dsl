module Tests.Validate where

import Layout.Ast
import Layout.Validate
import qualified Data.Map.Strict as M
import Test.HUnit (assertEqual, Test(TestCase, TestList))
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)


validateTests = testGroup "Validate Tests" $ hUnitTestToTests $ TestList $ map validateCompare
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
 where validateCompare (ast@(File decls), syms) = TestCase $ assertEqual
        ("validateCompare " ++ show (ast, syms))
        syms
        (buildSyms decls)
