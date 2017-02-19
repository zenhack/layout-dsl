{-# LANGUAGE FlexibleContexts, StandaloneDeriving, UndecidableInstances,
    ConstraintKinds #-}
{-|

Do some validation of the Ast, and collect some info useful for future
translation stages.
-}
module Layout.Validate
    ( buildSyms
    , SymbolTable(..)
    , ValidationError(..)
    , parseLayoutParams
    , checkLayoutParams
    )
where

import Control.Monad (forM_)
import Control.Monad.State (MonadState, get, put, state, runState)
import Control.Monad.Writer (MonadWriter, tell, WriterT(..))

import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Layout.Ast as Ast
import Layout.Parser (ParseStage)
import qualified Data.Map.Strict as M

data ValidationError
    = DuplicateDecl Text (ParseStage Ast.Decl)
    | OrphanDecl Text (ParseStage Ast.Decl)
    | NoSuchType Text
    | ArityMismatch Text
    | ConflictingLayoutParams [Ast.LayoutParam]
    | TypeParamsNotImplemented Text
    deriving(Show, Eq)

-- | A symbol table, mapping names to type, layout pairs.
newtype SymbolTable lParams slice
    = SymbolTable (M.Map Text (Ast.TypeDecl, Ast.LayoutDecl lParams slice))

deriving instance Ast.ParamCtx Show lParams slice => Show (SymbolTable lParams slice)
deriving instance Ast.ParamCtx Eq   lParams slice => Eq   (SymbolTable lParams slice)

-- | An "in progress" SymbolTable. In this case, we don't require at the type
-- level that there's a 1-1 type-to-layout mapping, since we use it while
-- building up the result.
data WorkingSymbolTable = WorkingSymbolTable
    { types   :: M.Map Text Ast.TypeDecl
    , layouts :: M.Map Text (ParseStage Ast.LayoutDecl)
    } deriving(Show, Eq)

-- | Convert a validated working symbol table to a finalized one. The working
-- symbol table *must* have the same set of keys in each of its maps. The
-- finalized symbol table enforces the 1-1 correspondence between layouts and
-- types at the type level.
finalizeSymbolTable :: WorkingSymbolTable -> ParseStage SymbolTable
finalizeSymbolTable syms = SymbolTable $ M.fromList $ map
    (\(k, t) -> (k, ( t
                    , fromJust (M.lookup k (layouts syms))
                    )))
    (M.toList (types syms))

-- | Compile a of declarations into the symbol table, validating it
-- along the way. Return a Left if any errors occur.
buildSyms :: [(Text, ParseStage Ast.Decl)] -> Either [ValidationError] (ParseStage SymbolTable)
buildSyms decls = case errs of
    [] -> Right (finalizeSymbolTable syms)
    _ -> Left errs
  where
    (((), errs), syms) = runM emptySyms $ do
        collectDeclsM decls
        checkOrphansM
        checkAritiesM
        checkNoTypeParams
    initM     = WriterT $ state $ \s -> (((), []), s)
    runM s m  = runState (runWriterT $ initM >> m) s
    emptySyms = WorkingSymbolTable { types   = M.empty
                                   , layouts = M.empty
                                   }


-- | Verify that the Ast contains no type parameters; we don't yet
-- implement these.
checkNoTypeParams :: (MonadState WorkingSymbolTable m, MonadWriter [ValidationError] m)
    => m ()
checkNoTypeParams = do
    syms <- M.toList . types <$> get
    forM_ syms $ \(k, v) -> case v of
        Ast.TypeDecl [] _ -> return ()
        _ -> tell [TypeParamsNotImplemented k]

-- | Collect declarations into the SymbolTable state. Errors are built up
-- using a MonadWriter.
collectDeclsM :: (MonadState WorkingSymbolTable m, MonadWriter [ValidationError] m)
    => [(Text, ParseStage Ast.Decl)] -> m ()
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

-- | parseLayoutParams parses its argument into a pair (align, byteorder)
-- representing the alignment and byte order specified by the params.
-- If either of these is unspecified, the corresponding component will be
-- 'Nothing'.
--
-- parseLayoutParams validates its argument, with invalid arguments resulting
-- in a Left.
parseLayoutParams :: [Ast.LayoutParam]
    -> Either [ValidationError] ( Maybe Int -- ^ alignment
                                , Maybe Ast.ByteOrder
                                )
parseLayoutParams = parseLayoutParams' [] (Nothing, Nothing)
parseLayoutParams' [] results [] =
    -- end of input without errors:
    Right results
parseLayoutParams' errs _ [] =
    -- end of input with errors:
    Left errs
parseLayoutParams' errs (Nothing, byteorder) (Ast.Align n:params) =
    -- Alignment when we don't already have one:
    parseLayoutParams' errs (Just n, byteorder) params
parseLayoutParams' errs (align, Nothing) (Ast.Endian byteorder:params) =
    -- Byte order when we don't already have one:
    parseLayoutParams' errs (align, Just byteorder) params
parseLayoutParams' errs results@(Just n, byteorder) (Ast.Align m:params) =
    -- Alignment when we already have one; this is an error, but
    -- keep going so we can get more info re: errors:
    parseLayoutParams'
        (ConflictingLayoutParams [Ast.Align n, Ast.Align m]:errs)
        results
        params
parseLayoutParams'
    errs
    results@(align, Just byteorder)
    (Ast.Endian byteorder':params) =
    -- Byte order when we already have one; this is an error, but
    -- keep going so we can get more info re: errors:
    parseLayoutParams'
        (ConflictingLayoutParams [ Ast.Endian byteorder
                                 , Ast.Endian byteorder'
                                 ]:errs)
        results
        params

type LPChecker (t :: * -> (* -> *) -> *) (slice :: * -> *) =
    t [Ast.LayoutParam] slice
    -> Either [ValidationError] (t (Maybe Int, Maybe Ast.ByteOrder) slice)

checkFileLPs        :: LPChecker Ast.File        slice
checkDeclLPs        :: LPChecker Ast.Decl        slice
checkLayoutDeclLPs  :: LPChecker Ast.LayoutDecl  slice
checkLayoutSpecLPs  :: LPChecker Ast.LayoutSpec  slice
checkLayoutFieldLPs :: LPChecker Ast.LayoutField slice
checkNamedLFLPs     :: LPChecker Ast.NamedLF     slice

checkLayoutParams = checkFileLPs

checkFileLPs (Ast.File decls) = do
    Ast.File <$> mapM (\(name, decl) -> do
                            decl' <- checkDeclLPs decl
                            return (name, decl'))
                 decls
checkDeclLPs (Ast.TypeD decl) = Right (Ast.TypeD decl)
checkDeclLPs (Ast.LayoutD decl) = Ast.LayoutD <$> checkLayoutDeclLPs decl
checkLayoutDeclLPs (Ast.LayoutDecl lParams specs) =
    Ast.LayoutDecl <$> parseLayoutParams lParams
                   <*> mapM checkLayoutSpecLPs specs
checkLayoutSpecLPs (Ast.LayoutSpec lParams field) =
    Ast.LayoutSpec <$> parseLayoutParams lParams
                   <*> checkLayoutFieldLPs field
checkLayoutFieldLPs (Ast.ConstLF field) = Right (Ast.ConstLF field)
checkLayoutFieldLPs (Ast.NamedLF name field) =
    Ast.NamedLF name <$> checkNamedLFLPs field
checkNamedLFLPs (Ast.SliceL field) = Right (Ast.SliceL field)
checkNamedLFLPs (Ast.StructL specs) = Ast.StructL <$> mapM checkLayoutSpecLPs specs
