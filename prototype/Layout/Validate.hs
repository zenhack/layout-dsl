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


-- | Compile a of declarations into the symbol table. Along the way, we detect
-- duplicate declarations, which are reported via the MonadWriter instance.
buildSyms :: (MonadState SymbolTable m, MonadWriter [ValidationError] m)
    => [(Text, Ast.Decl)] -> m ()
buildSyms [] = return ()
buildSyms ((name, decl):decls) = do
    syms <- get
    case decl of
        (Ast.TypeD tyDecl) -> do
            checkDups name (types syms) decl
            put $ syms { types = M.insert name tyDecl (types syms) }
        (Ast.LayoutD layoutDecl) -> do
            checkDups name (layouts syms) decl
            put $ syms { layouts = M.insert name layoutDecl (layouts syms) }
    buildSyms decls
  where
    checkDups name dict decl
        | name `M.member` dict = tell [DuplicateDecl name decl]
        | otherwise = return ()


-- | Return the arity (number of parameters) of a type declaration in the symbol
-- table, or Nothing if it is not present.
arity :: SymbolTable -> Text -> Maybe Int
arity syms name = do
    (Ast.TypeDecl params _) <- M.lookup name (types syms)
    return $ length params

-- | @checkArities syms typ@ Checks that all of the references to any type
-- by name in @typ@ (a) correspond to an actually declared type in the symbol
-- table, and (b) match the parameter count of the declaration.
checkArities :: (MonadWriter [ValidationError] m) => SymbolTable -> Ast.Type -> m ()
checkArities syms (Ast.NamedT name params)
    | arity syms name == Nothing = tell $ [NoSuchType name]
    | arity syms name /= Just (length params) = tell $ [ArityMismatch name]
checkArities syms (Ast.StructT fields) =
    mapM_ (checkArities syms . snd) fields
checkArities _ _ = return ()
