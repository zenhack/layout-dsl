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
    = DuplicateDecl Text Ast.Decl
    | OrphanDecl Ast.Decl
    | NoSuchType Text
    | ArityMismatch Text

data SymbolTable = SymbolTable
    { types   :: M.Map Text Ast.TypeDecl
    , layouts :: M.Map Text Ast.LayoutDecl
    } deriving(Show, Eq)

-- | Check for orphan declarations, i.e. type declarations without a
-- corresponding layout, or vice versa.
checkOrphans :: (MonadState SymbolTable m, MonadWriter [ValidationError] m) => m ()
checkOrphans = do
    SymbolTable types layouts <- get
    tell $ map OrphanDecl $
        (map Ast.TypeD   $ M.elems $ types   `M.difference` layouts) ++
        (map Ast.LayoutD $ M.elems $ layouts `M.difference` types)


-- | Sort a list of declarations into the symbol table.
sortDecls :: (MonadState SymbolTable m, MonadWriter [ValidationError] m)
    => [(Text, Ast.Decl)] -> m ()
sortDecls [] = return ()
sortDecls ((name, decl):decls) = do
    syms <- get
    case decl of
        (Ast.TypeD tyDecl) -> do
            checkDups name (types syms) decl
            put $ syms { types = M.insert name tyDecl (types syms) }
        (Ast.LayoutD layoutDecl) -> do
            checkDups name (layouts syms) decl
            put $ syms { layouts = M.insert name layoutDecl (layouts syms) }
    sortDecls decls
  where
    checkDups name dict decl
        | name `M.member` dict = tell [DuplicateDecl name decl]
        | otherwise = return ()



-- | @arities types@ is a map from names of type declarations to the lengths of
-- their parameter lists.
arities :: M.Map Text Ast.TypeDecl -> M.Map Text Int
arities types = M.map arity types where
    arity (Ast.TypeDecl params _) = length params

-- | @checkArities arities typ@ Checks that all of the references to any type
-- by name in @typ@ (a) correspond to an actually declared type, and (b) match
-- the parameter count of the declaration.
--
-- @arities@ is a map from the names of the types to their arities.
checkArities :: (MonadWriter [ValidationError] m) => M.Map Text Int -> Ast.Type -> m ()
checkArities arities (Ast.NamedT name params)
    | M.lookup name arities /= Just (length params) = tell $ [ArityMismatch name]
    | M.lookup name arities == Nothing = tell $ [NoSuchType name]
checkArities arities (Ast.StructT fields) =
    mapM_ (checkArities arities . snd) fields
checkArities _ _ = return ()
