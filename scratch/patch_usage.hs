import Control.Monad.State

type LastUseState = M.Map Ident Int

markLastUseModule :: Module Ann -> State LastUseState (Module Ann)
markLastUseModule m = do
  decls' <- markLastUseBinds (moduleDecls m)
  pure $ m { moduleDecls = decls' }

markLastUseBinds :: [Bind Ann] -> State LastUseState [Bind Ann]
markLastUseBinds [] = pure []
markLastUseBinds (b:bs) = do
  b' <- markLastUseBind b
  bs' <- markLastUseBinds bs
  pure (b':bs')

markLastUseBind :: Bind Ann -> State LastUseState (Bind Ann)
markLastUseBind (NonRec ann ident expr) = do
  expr' <- markLastUseExpr expr
  let count = case ann of (_, _, _, _, Just (c, _)) -> c; _ -> -1
  modify (M.insert ident count)
  pure (NonRec ann ident expr')

markLastUseBind (Rec recBinds) = do
  forM_ recBinds $ \((ann, ident), _) -> do
    let count = case ann of (_, _, _, _, Just (c, _)) -> c; _ -> -1
    modify (M.insert ident count)
  recBinds' <- forM recBinds $ \((ann, ident), expr) -> do
    expr' <- markLastUseExpr expr
    pure ((ann, ident), expr')
  pure (Rec recBinds')

markLastUseExpr :: Expr Ann -> State LastUseState (Expr Ann)
markLastUseExpr (Literal ann lit) = do
  lit' <- case lit of
    ArrayLiteral arr -> ArrayLiteral <$> mapM markLastUseExpr arr
    ObjectLiteral obj -> ObjectLiteral <$> mapM (\(k, e) -> (,) k <$> markLastUseExpr e) obj
    _ -> pure lit
  pure (Literal ann lit')

markLastUseExpr (Constructor ann ty ctor fields) = do
  let ann' = setUsageCount 1 False ann
  pure (Constructor ann' ty ctor fields)

markLastUseExpr (Accessor ann field expr) = do
  expr' <- markLastUseExpr expr
  pure (Accessor ann field expr')

markLastUseExpr (ObjectUpdate ann expr keys updates) = do
  expr' <- markLastUseExpr expr
  updates' <- mapM (\(k, e) -> (,) k <$> markLastUseExpr e) updates
  pure (ObjectUpdate ann expr' keys updates')

markLastUseExpr (Abs ann ident body) = do
  oldState <- get
  let count = case ann of (_, _, _, _, Just (c, _)) -> c; _ -> -1
  modify (M.insert ident count)
  body' <- markLastUseExpr body
  modify (\s -> case M.lookup ident oldState of Just v -> M.insert ident v s; Nothing -> M.delete ident s)
  pure (Abs ann ident body')

markLastUseExpr (App ann f x) = do
  f' <- markLastUseExpr f
  x' <- markLastUseExpr x
  let ann' = setUsageCount 1 False ann
  pure (App ann' f' x')

markLastUseExpr (Var ann q) = do
  s <- get
  case q of
    Qualified _ ident -> case M.lookup ident s of
      Just c -> do
        let c' = if c == -1 then -1 else c - 1
        modify (M.insert ident c')
        let ann' = if c' == 0 then setUsageCount 1 False ann else ann
        pure (Var ann' q)
      Nothing -> pure (Var ann q)

markLastUseExpr (Case ann exprs alts) = do
  exprs' <- mapM markLastUseExpr exprs
  oldState <- get
  altResults <- forM alts $ \alt -> do
    put oldState
    alt' <- markLastUseAlt alt
    newState <- get
    pure (alt', newState)
  let (alts', states) = unzip altResults
      finalState = foldr (M.intersectionWith min) oldState states
  put finalState
  pure (Case ann exprs' alts')

markLastUseExpr (Let ann binds expr) = do
  oldState <- get
  binds' <- markLastUseBinds binds
  expr' <- markLastUseExpr expr
  let boundNames = concatMap bindNames binds'
  modify (\s -> foldr (\k s' -> case M.lookup k oldState of Just v -> M.insert k v s'; Nothing -> M.delete k s') s boundNames)
  pure (Let ann binds' expr')

markLastUseExpr (TypeApp ann expr ty) = do
  expr' <- markLastUseExpr expr
  pure (TypeApp ann expr' ty)

markLastUseAlt :: CaseAlternative Ann -> State LastUseState (CaseAlternative Ann)
markLastUseAlt (CaseAlternative binders result) = do
  oldState <- get
  let extractCounts (VarBinder ann ident) = [(ident, case ann of (_, _, _, _, Just (c, _)) -> c; _ -> -1)]
      extractCounts (NamedBinder ann ident b) = (ident, case ann of (_, _, _, _, Just (c, _)) -> c; _ -> -1) : extractCounts b
      extractCounts (ConstructorBinder _ _ _ bs) = concatMap extractCounts bs
      extractCounts _ = []
      counts = concatMap extractCounts binders
  forM_ counts $ \(ident, count) -> modify (M.insert ident count)
  
  result' <- case result of
    Right expr -> Right <$> markLastUseExpr expr
    Left guards -> Left <$> mapM (\(g, e) -> (,) <$> markLastUseExpr g <*> markLastUseExpr e) guards
    
  modify (\s -> foldr (\(k,_) s' -> case M.lookup k oldState of Just v -> M.insert k v s'; Nothing -> M.delete k s') s counts)
  pure (CaseAlternative binders result')

bindNames :: Bind a -> [Ident]
bindNames (NonRec _ ident _) = [ident]
bindNames (Rec binds) = map (\((_, ident), _) -> ident) binds
