{-# LANGUAGE FlexibleContexts #-}
{-|

Do some validation of the Ast, and collect some info useful for future
translation stages.
-}
module Layout.Validate where

import Control.Monad.State (MonadState, get, put, state, runState)
import Control.Monad.Writer (MonadWriter, tell, WriterT(..))

import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Layout.Ast as Ast

import qualified Data.Map.Strict as M

data ValidationError
    = DuplicateDecl Text Ast.Decl
    | OrphanDecl Text Ast.Decl
    | NoSuchType Text
    | ArityMismatch Text
    deriving(Show, Eq)

-- | A symbol table, mapping names to type, layout pairs.
newtype SymbolTable = SymbolTable (M.Map Text (Ast.TypeDecl, Ast.LayoutDecl))
                    deriving(Show, Eq)

-- | An "in progress" SymbolTable. In this case, we don't require at the type
-- level that there's a 1-1 type-to-layout mapping, since we use it while
-- building up the result.
data WorkingSymbolTable = WorkingSymbolTable
    { types   :: M.Map Text Ast.TypeDecl
    , layouts :: M.Map Text Ast.LayoutDecl
    } deriving(Show, Eq)

-- | Convert a validated working symbol table to a finalized one. The working
-- symbol table *must* have the same set of keys in each of its maps. The
-- finalized symbol table enforces the 1-1 correspondence between layouts and
-- types at the type level.
finalizeSymbolTable :: WorkingSymbolTable -> SymbolTable
finalizeSymbolTable syms = SymbolTable $ M.fromList $ map
    (\(k, t) -> (k, ( t
                    , fromJust (M.lookup k (layouts syms))
                    )))
    (M.toList (types syms))

-- | Compile a of declarations into the symbol table, validating it
-- along the way. Return a Left if any errors occur.
buildSyms :: [(Text, Ast.Decl)] -> Either [ValidationError] SymbolTable
buildSyms decls = case errs of
    [] -> Right (finalizeSymbolTable syms)
    _ -> Left errs
  where
    (((), errs), syms) = runM emptySyms $ do
        collectDeclsM decls
        checkOrphansM
        checkAritiesM
    initM     = WriterT $ state $ \s -> (((), []), s)
    runM s m  = runState (runWriterT $ initM >> m) s
    emptySyms = WorkingSymbolTable { types   = M.empty
                                   , layouts = M.empty
                                   }

-- | Collect declarations into the SymbolTable state. Errors are built up
-- using a MonadWriter.
collectDeclsM :: (MonadState WorkingSymbolTable m, MonadWriter [ValidationError] m)
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
checkOrphansM :: (MonadState WorkingSymbolTable m, MonadWriter [ValidationError] m) => m ()
checkOrphansM = do
    WorkingSymbolTable types layouts <- get
    tell $ map (uncurry OrphanDecl) $
        (M.toList $ M.map Ast.TypeD   $ types   `M.difference` layouts) ++
        (M.toList $ M.map Ast.LayoutD $ layouts `M.difference` types)

-- | Return the arity (number of parameters) of a type declaration in the symbol
-- table, or Nothing if it is not present.
arity :: WorkingSymbolTable -> Text -> Maybe Int
arity syms name = do
    (Ast.TypeDecl params _) <- M.lookup name (types syms)
    return $ length params

checkAritiesM = do
    syms@(WorkingSymbolTable types _) <- get
    mapM_ (\(Ast.TypeDecl _ ty) -> checkArities syms ty) (M.elems types)

-- | @checkArities syms typ@ Checks that all of the references to any type
-- by name in @typ@ (a) correspond to an actually declared type in the symbol
-- table, and (b) match the parameter count of the declaration.
checkArities :: (MonadWriter [ValidationError] m) => WorkingSymbolTable -> Ast.Type -> m ()
checkArities syms (Ast.NamedT name params)
    | arity syms name == Nothing = tell $ [NoSuchType name]
    | arity syms name /= Just (length params) = tell $ [ArityMismatch name]
checkArities syms (Ast.StructT fields) =
    mapM_ (checkArities syms . snd) fields
checkArities _ _ = return ()
