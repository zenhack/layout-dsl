{-# LANGUAGE FlexibleContexts #-}
{-|

Do some validation of the Ast, and collect some info useful for future
translation stages.
-}
module Layout.Validate where

import Control.Monad.State (MonadState, get, put, StateT(..))
import Control.Monad.Writer (MonadWriter, tell, WriterT(..))
import Control.Monad.Identity (Identity(..))

import Data.Text(Text)
import qualified Layout.Ast as Ast

import qualified Data.Map.Strict as M

data ValidationError
    = DuplicateDecl Text Ast.Decl
    | OrphanDecl Text Ast.Decl
    | NoSuchType Text
    | ArityMismatch Text
    deriving(Show, Eq)

data SymbolTable = SymbolTable
    { types   :: M.Map Text Ast.TypeDecl
    , layouts :: M.Map Text Ast.LayoutDecl
    } deriving(Show, Eq)

-- | Compile a of declarations into the symbol table, validating it
-- along the way. Return a Left if any errors occur.
buildSyms :: [(Text, Ast.Decl)] -> Either [ValidationError] SymbolTable
buildSyms decls = case errs of
    [] -> Right syms
    _ -> Left errs
  where
    (((), errs), syms) = runM emptySyms $ do
        collectDeclsM decls
        checkOrphansM
        checkAritiesM
    initM     = WriterT $ StateT $ \s -> Identity (((), []), s)
    runM s m  = runIdentity $ runStateT (runWriterT $ initM >> m) s
    emptySyms = SymbolTable { types   = M.empty
                            , layouts = M.empty
                            }

-- | Collect declarations into the SymbolTable state. Errors are built up
-- using a MonadWriter.
collectDeclsM :: (MonadState SymbolTable m, MonadWriter [ValidationError] m)
    => [(Text, Ast.Decl)] -> m ()
collectDeclsM [] = return ()
collectDeclsM ((name, decl):decls) = do
    syms <- get
    case decl of
        (Ast.TypeD tyDecl) -> do
            checkDups name (types syms) decl
            put $ syms { types = M.insert name tyDecl (types syms) }
        (Ast.LayoutD layoutDecl) -> do
            checkDups name (layouts syms) decl
            put $ syms { layouts = M.insert name layoutDecl (layouts syms) }
    collectDeclsM decls
  where
    checkDups name dict decl
        | name `M.member` dict = tell [DuplicateDecl name decl]
        | otherwise = return ()


-- | Check for orphan declarations in the symbol table, i.e. type declarations
-- without a corresponding layout, or vice versa.
checkOrphansM :: (MonadState SymbolTable m, MonadWriter [ValidationError] m) => m ()
checkOrphansM = do
    SymbolTable types layouts <- get
    tell $ map (uncurry OrphanDecl) $
        (M.toList $ M.map Ast.TypeD   $ types   `M.difference` layouts) ++
        (M.toList $ M.map Ast.LayoutD $ layouts `M.difference` types)

-- | Return the arity (number of parameters) of a type declaration in the symbol
-- table, or Nothing if it is not present.
arity :: SymbolTable -> Text -> Maybe Int
arity syms name = do
    (Ast.TypeDecl params _) <- M.lookup name (types syms)
    return $ length params

checkAritiesM = do
    syms@(SymbolTable types _) <- get
    mapM_ (\(Ast.TypeDecl _ ty) -> checkArities syms ty) (M.elems types)

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
