-- | Local usage facts with separate identity resolution, bounds and backward
-- | liveness analyses. These facts do not establish ownership of heap objects.
module Language.PureScript.CoreFn.Usage.Analysis (annotateUsage) where

import Prelude

import Control.Monad.State.Strict (State, evalState, get, put, runState, state)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as S

import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.CoreFn.Ann
import Language.PureScript.CoreFn.Binders
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Module
import Language.PureScript.Names (Ident, Qualified(..), QualifiedBy(..))

type Scope = M.Map Ident BindingId

type Uses = M.Map BindingId (Maybe Integer, Bool)

type Live = S.Set BindingId

annotateUsage :: Module Ann -> Module Ann
annotateUsage m =
  let numbered = evalState (mapM numberTopBind (moduleDecls m)) 0
      counted = map countTopBind numbered
      blocked = S.fromList
        [ bindingUsageId info
        | ann <- concatMap bindAnnotations counted
        , Just info <- [bindingUsage (usageInfo ann)]
        , bindingMaxUses info == Nothing
        ]
  in m { moduleDecls = map (lastTopBind blocked) counted }

usageInfo :: Ann -> UsageInfo
usageInfo (_, _, _, _, info) = fromMaybe emptyUsageInfo info

updateUsage :: (UsageInfo -> UsageInfo) -> Ann -> Ann
updateUsage f (ss, comments, ty, meta, info) =
  (ss, comments, ty, meta, Just (f (fromMaybe emptyUsageInfo info)))

bindingId :: Ann -> Maybe BindingId
bindingId = fmap bindingUsageId . bindingUsage . usageInfo

occurrenceId :: Ann -> Maybe BindingId
occurrenceId = fmap variableBindingId . variableUse . usageInfo

freshBinding :: Ann -> State Int Ann
freshBinding ann = do
  next <- get
  put (next + 1)
  pure $ updateUsage (\info -> info
    { bindingUsage = Just (BindingUsage (BindingId next) Nothing Nothing) }) ann

extendScope :: Ident -> Ann -> Scope -> Scope
extendScope name ann scope = maybe scope (\key -> M.insert name key scope) (bindingId ann)

-- Module declarations and foreign values are not local instances. Their uses
-- outside this module cannot be bounded by this pass.
numberTopBind :: Bind Ann -> State Int (Bind Ann)
numberTopBind (NonRec ann name expr) = NonRec ann name <$> numberExpr M.empty expr
numberTopBind (Rec binds) = Rec <$> mapM (\(key, expr) -> (key,) <$> numberExpr M.empty expr) binds

numberExpr :: Scope -> Expr Ann -> State Int (Expr Ann)
numberExpr scope = \case
  Literal ann lit -> Literal ann <$> traverseLiteral (numberExpr scope) lit
  Constructor ann ty ctor fields -> pure (Constructor ann ty ctor fields)
  Accessor ann field expr -> Accessor ann field <$> numberExpr scope expr
  ObjectUpdate ann expr keys updates -> ObjectUpdate ann
    <$> numberExpr scope expr <*> pure keys
    <*> mapM (\(key, value) -> (key,) <$> numberExpr scope value) updates
  Abs ann name body -> do
    ann' <- freshBinding ann
    Abs ann' name <$> numberExpr (extendScope name ann' scope) body
  App ann f x -> App ann <$> numberExpr scope f <*> numberExpr scope x
  Var ann q@(Qualified qualifier name) -> case qualifier of
    ByModuleName _ -> pure (Var ann q)
    BySourcePos _ -> pure $ Var
      (maybe ann (\key -> updateUsage (\info -> info
        { variableUse = Just (VariableUse key UnknownLastLocalUse) }) ann) (M.lookup name scope)) q
  Case ann exprs alts -> Case ann <$> mapM (numberExpr scope) exprs <*> mapM (numberAlt scope) alts
  Let ann binds body -> do
    (binds', scope') <- numberBinds scope binds
    Let ann binds' <$> numberExpr scope' body
  TypeApp ann expr ty -> TypeApp ann <$> numberExpr scope expr <*> pure ty

numberBinds :: Scope -> [Bind Ann] -> State Int ([Bind Ann], Scope)
numberBinds scope [] = pure ([], scope)
numberBinds scope (NonRec ann name expr : rest) = do
  expr' <- numberExpr scope expr
  ann' <- freshBinding ann
  (rest', scope') <- numberBinds (extendScope name ann' scope) rest
  pure (NonRec ann' name expr' : rest', scope')
numberBinds scope (Rec binds : rest) = do
  keys <- mapM (\((ann, name), _) -> (, name) <$> freshBinding ann) binds
  let recursiveScope = foldr (\(ann, name) -> extendScope name ann) scope keys
  exprs <- mapM (numberExpr recursiveScope . snd) binds
  (rest', scope') <- numberBinds recursiveScope rest
  pure (Rec (zip keys exprs) : rest', scope')

numberAlt :: Scope -> CaseAlternative Ann -> State Int (CaseAlternative Ann)
numberAlt scope (CaseAlternative binders result) = do
  (binders', scope') <- numberBinders scope binders
  result' <- case result of
    Right expr -> Right <$> numberExpr scope' expr
    Left guards -> Left <$> mapM (\(guard, expr) -> (,)
      <$> numberExpr scope' guard <*> numberExpr scope' expr) guards
  pure (CaseAlternative binders' result')

numberBinders :: Scope -> [Binder Ann] -> State Int ([Binder Ann], Scope)
numberBinders scope [] = pure ([], scope)
numberBinders scope (binder : rest) = do
  (binder', scope') <- numberBinder scope binder
  (rest', scope'') <- numberBinders scope' rest
  pure (binder' : rest', scope'')

numberBinder :: Scope -> Binder Ann -> State Int (Binder Ann, Scope)
numberBinder scope = \case
  VarBinder ann name -> do
    ann' <- freshBinding ann
    pure (VarBinder ann' name, extendScope name ann' scope)
  NamedBinder ann name inner -> do
    ann' <- freshBinding ann
    (inner', scope') <- numberBinder (extendScope name ann' scope) inner
    pure (NamedBinder ann' name inner', scope')
  ConstructorBinder ann ty ctor binders -> do
    (binders', scope') <- numberBinders scope binders
    pure (ConstructorBinder ann ty ctor binders', scope')
  LiteralBinder ann (ArrayLiteral binders) -> do
    (binders', scope') <- numberBinders scope binders
    pure (LiteralBinder ann (ArrayLiteral binders'), scope')
  LiteralBinder ann (ObjectLiteral fields) -> do
    (binders', scope') <- numberBinders scope (map snd fields)
    pure (LiteralBinder ann (ObjectLiteral (zip (map fst fields) binders')), scope')
  binder -> pure (binder, scope)

-- Bounds refer to a binding instance, not to aliases of its value. Integer
-- avoids silently overflowing a count into an unsafe smaller bound.
mergeUses :: (Integer -> Integer -> Integer) -> Uses -> Uses -> Uses
mergeUses combine = M.unionWith (\(a, ea) (b, eb) -> (combine <$> a <*> b, ea || eb))

sumUses :: [Uses] -> Uses
sumUses = foldr (mergeUses (+)) M.empty

setBound :: Uses -> Ann -> Ann
setBound uses ann = case bindingUsage (usageInfo ann) of
  Nothing -> ann
  Just info ->
    let (count, escaping) = M.findWithDefault (Just 0, False) (bindingUsageId info) uses
    in updateUsage (\current -> current
      { bindingUsage = Just (info { bindingMaxUses = count, bindingEscapingContext = Just escaping }) }) ann

without :: [BindingId] -> Uses -> Uses
without keys uses = foldr M.delete uses keys

countTopBind :: Bind Ann -> Bind Ann
countTopBind (NonRec ann name expr) = NonRec ann name (fst (countExpr True expr))
countTopBind (Rec binds) = Rec (map (\(key, expr) -> (key, fst (countExpr True expr))) binds)

countExpr :: Bool -> Expr Ann -> (Expr Ann, Uses)
countExpr escaping = \case
  Literal ann lit ->
    let step expr = state $ \accumulatedUses ->
          let (expr', more) = countExpr escaping expr
          in (expr', mergeUses (+) accumulatedUses more)
        (lit', uses) = runState (traverseLiteral step lit) M.empty
    in (Literal ann lit', uses)
  expr@(Constructor _ _ _ _) -> (expr, M.empty)
  Accessor ann field expr ->
    let (expr', uses) = countExpr escaping expr
    in (Accessor ann field expr', uses)
  ObjectUpdate ann expr keys updates ->
    let (expr', baseUses) = countExpr escaping expr
        (values, updateUses) = unzip (map (countExpr escaping . snd) updates)
    in (ObjectUpdate ann expr' keys (zip (map fst updates) values), sumUses (baseUses : updateUses))
  Abs ann name body ->
    let (body', bodyUses) = countExpr True body
        freeUses = without (maybe [] pure (bindingId ann)) bodyUses
        captured = M.map (\(_, bodyEscaping) -> (Nothing, escaping || bodyEscaping)) freeUses
    in (Abs (setBound bodyUses ann) name body', captured)
  App ann f x ->
    let (f', fu) = countExpr True f
        (x', xu) = countExpr True x
    in (App ann f' x', mergeUses (+) fu xu)
  expr@(Var ann _) ->
    (expr, maybe M.empty (\key -> M.singleton key (Just 1, escaping)) (occurrenceId ann))
  Case ann exprs alts ->
    let (exprs', scrutineeUses) = unzip (map (countExpr False) exprs)
        (alts', alternativeUses) = unzip (map (countAlt escaping) alts)
        hasGuards (CaseAlternative _ (Left _)) = True
        hasGuards _ = False
        combine = if any hasGuards alts then (+) else max
        alternatives = foldr (mergeUses combine) M.empty alternativeUses
    in (Case ann exprs' alts', mergeUses (+) (sumUses scrutineeUses) alternatives)
  Let ann binds body ->
    let (body', bodyUses) = countExpr escaping body
        (binds', uses) = countBinds binds bodyUses
    in (Let ann binds' body', uses)
  TypeApp ann expr ty ->
    let (expr', uses) = countExpr escaping expr
    in (TypeApp ann expr' ty, uses)

countBinds :: [Bind Ann] -> Uses -> ([Bind Ann], Uses)
countBinds [] uses = ([], uses)
countBinds (bind : rest) uses =
  let (rest', nextUses) = countBinds rest uses
      (bind', allUses) = countBind bind nextUses
  in (bind' : rest', allUses)

countBind :: Bind Ann -> Uses -> (Bind Ann, Uses)
countBind (NonRec ann name expr) nextUses =
  let keys = maybe [] pure (bindingId ann)
      escaping = maybe True (\key -> snd (M.findWithDefault (Just 0, False) key nextUses)) (bindingId ann)
      (expr', exprUses) = countExpr escaping expr
  in (NonRec (setBound nextUses ann) name expr', mergeUses (+) exprUses (without keys nextUses))
countBind (Rec binds) nextUses =
  let (exprs, uses) = unzip (map (countExpr True . snd) binds)
      repeated = M.map (\(_, escaping) -> (Nothing, escaping)) (sumUses uses)
      total = mergeUses (+) nextUses repeated
      keys = mapMaybe (bindingId . fst . fst) binds
      annotated = zipWith (\((ann, name), _) expr -> ((setBound total ann, name), expr)) binds exprs
  in (Rec annotated, without keys total)

countAlt :: Bool -> CaseAlternative Ann -> (CaseAlternative Ann, Uses)
countAlt escaping (CaseAlternative binders result) =
  let (result', uses) = case result of
        Right expr -> let (expr', more) = countExpr escaping expr in (Right expr', more)
        Left guards ->
          let countGuard (guard, expr) =
                let (guard', gu) = countExpr False guard
                    (expr', eu) = countExpr escaping expr
                in ((guard', expr'), mergeUses (+) gu eu)
              (guards', guardUses) = unzip (map countGuard guards)
          in (Left guards', sumUses guardUses)
      ids = concatMap patternIds binders
  in (CaseAlternative (map (fmap (setBound uses)) binders) result', without ids uses)

-- Backward may-liveness follows success and failure continuations separately.
-- Captured/unknown-count bindings are blocked everywhere: a lexical last read
-- cannot prove that a reusable closure will not read that instance again.
lastTopBind :: Live -> Bind Ann -> Bind Ann
lastTopBind blocked (NonRec ann name expr) = NonRec ann name (fst (lastExpr blocked S.empty expr))
lastTopBind blocked (Rec binds) = Rec (map (\(key, expr) -> (key, fst (lastExpr blocked S.empty expr))) binds)

lastExpr :: Live -> Live -> Expr Ann -> (Expr Ann, Live)
lastExpr blocked after = \case
  Literal ann lit ->
    let (values, before) = lastExprs blocked after (literalValues lit)
    in (Literal ann (replaceLiteralValues lit values), before)
  expr@(Constructor _ _ _ _) -> (expr, after)
  Accessor ann field expr ->
    let (expr', before) = lastExpr blocked after expr
    in (Accessor ann field expr', before)
  ObjectUpdate ann expr keys updates ->
    let (values, beforeUpdates) = lastExprs blocked after (map snd updates)
        (expr', before) = lastExpr blocked beforeUpdates expr
    in (ObjectUpdate ann expr' keys (zip (map fst updates) values), before)
  Abs ann name body ->
    let (body', beforeBody) = lastExpr blocked S.empty body
        free = maybe beforeBody (`S.delete` beforeBody) (bindingId ann)
    in (Abs ann name body', S.union after free)
  App ann f x ->
    let (x', beforeX) = lastExpr blocked after x
        (f', before) = lastExpr blocked beforeX f
    in (App ann f' x', before)
  expr@(Var ann q) -> case variableUse (usageInfo ann) of
    Nothing -> (expr, after)
    Just info ->
      let key = variableBindingId info
          proof = if S.member key after || S.member key blocked then UnknownLastLocalUse else ProvenLastLocalUse
          ann' = updateUsage (\current -> current
            { variableUse = Just (info { variableLastLocalUse = proof }) }) ann
      in (Var ann' q, S.insert key after)
  Case ann exprs alts ->
    let (alts', beforeAlts) = lastAlts blocked after alts
        (exprs', before) = lastExprs blocked beforeAlts exprs
    in (Case ann exprs' alts', before)
  Let ann binds body ->
    let (body', beforeBody) = lastExpr blocked after body
        (binds', before) = lastBinds blocked beforeBody binds
    in (Let ann binds' body', before)
  TypeApp ann expr ty ->
    let (expr', before) = lastExpr blocked after expr
    in (TypeApp ann expr' ty, before)

lastExprs :: Live -> Live -> [Expr Ann] -> ([Expr Ann], Live)
lastExprs blocked = mapBackwards (lastExpr blocked)

mapBackwards :: (s -> a -> (b, s)) -> s -> [a] -> ([b], s)
mapBackwards _ after [] = ([], after)
mapBackwards f after (item : rest) =
  let (rest', beforeRest) = mapBackwards f after rest
      (item', before) = f beforeRest item
  in (item' : rest', before)

lastAlts :: Live -> Live -> [CaseAlternative Ann] -> ([CaseAlternative Ann], Live)
lastAlts blocked after = mapBackwards step after
  where
  step fallback (CaseAlternative binders result) =
    let (result', beforeResult) = case result of
          Right expr -> let (expr', before) = lastExpr blocked after expr in (Right expr', before)
          Left guards ->
            let guardStep next (guard, expr) =
                  let (expr', beforeBody) = lastExpr blocked after expr
                      (guard', beforeGuard) = lastExpr blocked (S.union beforeBody next) guard
                  in ((guard', expr'), beforeGuard)
                (guards', before) = mapBackwards guardStep fallback guards
            in (Left guards', before)
        beforePattern = foldr S.delete beforeResult (concatMap patternIds binders)
    -- Pattern failure bypasses this result and reaches a later alternative.
    in (CaseAlternative binders result', S.union fallback beforePattern)

lastBinds :: Live -> Live -> [Bind Ann] -> ([Bind Ann], Live)
lastBinds blocked = mapBackwards step
  where
  step after (NonRec ann name expr) =
    let beforeBinding = maybe after (`S.delete` after) (bindingId ann)
        (expr', before) = lastExpr blocked beforeBinding expr
    in (NonRec ann name expr', before)
  step after (Rec binds) =
    let ids = mapMaybe (bindingId . fst . fst) binds
        group = S.fromList ids
        (exprs, before) = lastExprs (S.union blocked group) (S.union after group) (map snd binds)
    in (Rec (zip (map fst binds) exprs), foldr S.delete before ids)

patternIds :: Binder Ann -> [BindingId]
patternIds = mapMaybe bindingId . patternAnnotations

patternAnnotations :: Binder Ann -> [Ann]
patternAnnotations = \case
  NullBinder ann -> [ann]
  VarBinder ann _ -> [ann]
  NamedBinder ann _ inner -> ann : patternAnnotations inner
  ConstructorBinder ann _ _ binders -> ann : concatMap patternAnnotations binders
  LiteralBinder ann lit -> ann : concatMap patternAnnotations (literalValues lit)

bindAnnotations :: Bind Ann -> [Ann]
bindAnnotations (NonRec ann _ expr) = ann : exprAnnotations expr
bindAnnotations (Rec binds) = concatMap (\((ann, _), expr) -> ann : exprAnnotations expr) binds

exprAnnotations :: Expr Ann -> [Ann]
exprAnnotations expr = extractAnn expr : case expr of
  Literal _ lit -> concatMap exprAnnotations (literalValues lit)
  Constructor _ _ _ _ -> []
  Accessor _ _ value -> exprAnnotations value
  ObjectUpdate _ value _ fields -> exprAnnotations value <> concatMap (exprAnnotations . snd) fields
  Abs _ _ body -> exprAnnotations body
  App _ f x -> exprAnnotations f <> exprAnnotations x
  Var _ _ -> []
  Case _ values alts -> concatMap exprAnnotations values <> concatMap altAnnotations alts
  Let _ binds body -> concatMap bindAnnotations binds <> exprAnnotations body
  TypeApp _ value _ -> exprAnnotations value
  where
  altAnnotations (CaseAlternative binders result) = concatMap patternAnnotations binders <>
    either (concatMap (\(guard, value) -> exprAnnotations guard <> exprAnnotations value)) exprAnnotations result

literalValues :: Literal a -> [a]
literalValues (ArrayLiteral values) = values
literalValues (ObjectLiteral fields) = map snd fields
literalValues _ = []

replaceLiteralValues :: Literal a -> [a] -> Literal a
replaceLiteralValues (ArrayLiteral _) values = ArrayLiteral values
replaceLiteralValues (ObjectLiteral fields) values = ObjectLiteral (zip (map fst fields) values)
replaceLiteralValues lit _ = lit

traverseLiteral :: Applicative f => (a -> f b) -> Literal a -> f (Literal b)
traverseLiteral f = \case
  NumericLiteral n -> pure (NumericLiteral n)
  StringLiteral s -> pure (StringLiteral s)
  CharLiteral c -> pure (CharLiteral c)
  BooleanLiteral b -> pure (BooleanLiteral b)
  ArrayLiteral values -> ArrayLiteral <$> traverse f values
  ObjectLiteral fields -> ObjectLiteral <$> traverse (\(key, value) -> (key,) <$> f value) fields
