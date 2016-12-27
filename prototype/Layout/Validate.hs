{-# LANGUAGE FlexibleContexts #-}
{-|

Do some validation of the Ast, and collect some info useful for future
translation stages.
-}
module Layout.Validate where

import Control.Monad.State (MonadState, get, put)
import Control.Monad.Writer (MonadWriter, tell)

import Data.Text(Text)
import qualified Layout.Ast as Ast

import qualified Data.Map.Strict as M

data ValidationError
    = DuplicateDecl Ast.Decl
    | OrphanDecl Ast.Decl

type DeclMap = M.Map Text Ast.Decl

-- | Check for orphan declarations, i.e. type declarations without a
-- corresponding layout, or vice versa.
checkOrphans :: (MonadState (DeclMap, DeclMap) m, MonadWriter [ValidationError] m) => m ()
checkOrphans = do
    (types, layouts) <- get
    tell $ map OrphanDecl $
        (M.elems $ types   `M.difference` layouts) ++
        (M.elems $ layouts `M.difference` types)


-- | Sort a list of declarations into the two maps held in the state.
-- type declarations go in the first map, while layout declarations go in the
-- second.
sortDecls :: (MonadState (DeclMap, DeclMap) m, MonadWriter [ValidationError] m)
    => [Ast.Decl] -> m ()
sortDecls [] = return ()
sortDecls (decl:decls) = do
    (types, layouts) <- get
    case decl of
        (Ast.TypeDecl name _ _) -> do
            checkDups name types decl
            put (M.insert name decl types, layouts)
        (Ast.LayoutDecl name _ _) -> do
            checkDups name layouts decl
            put (types, M.insert name decl layouts)
    sortDecls decls
  where
    checkDups name dict decl
        | name `M.member` dict = tell [DuplicateDecl decl]
        | otherwise = return ()
