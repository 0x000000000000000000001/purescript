module Language.PureScript.CoreFn.Usage (computeUsage) where

import Prelude
import Data.Map.Strict qualified as M
import Language.PureScript.Names (pattern ByNullSourcePos, Ident, Qualified(..), QualifiedBy(..))
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Binders
import Language.PureScript.CoreFn.Module
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.AST.Literals (Literal(..))

type UsageMap = M.Map Ident (Int, Bool)

addCount :: Int -> Int -> Int
addCount (-1) _ = -1
addCount _ (-1) = -1
addCount a b = a + b

maxCount :: Int -> Int -> Int
maxCount (-1) _ = -1
maxCount _ (-1) = -1
maxCount a b = max a b

mergeUsages :: UsageMap -> UsageMap -> UsageMap
mergeUsages = M.unionWith (\(c1, e1) (c2, e2) -> (addCount c1 c2, e1 || e2))

mergeUsagesList :: [UsageMap] -> UsageMap
mergeUsagesList = foldr mergeUsages M.empty

mergeUsagesMax :: UsageMap -> UsageMap -> UsageMap
mergeUsagesMax = M.unionWith (\(c1, e1) (c2, e2) -> (maxCount c1 c2, e1 || e2))

mergeUsagesListMax :: [UsageMap] -> UsageMap
mergeUsagesListMax = foldr mergeUsagesMax M.empty

setUsageCount :: Int -> Bool -> Ann -> Ann
setUsageCount c e (ss, com, ty, meta, _) = (ss, com, ty, meta, Just (c, e))

computeUsage :: Module Ann -> Module Ann
computeUsage m = 
  let (decls', _) = computeUsageBindsLocal True (moduleDecls m) M.empty
  in m { moduleDecls = decls' }

computeUsageBindsLocal :: Bool -> [Bind Ann] -> UsageMap -> ([Bind Ann], UsageMap)
computeUsageBindsLocal _ [] m = ([], m)
computeUsageBindsLocal ctx (b:bs) mBody =
  let (bs', mNext) = computeUsageBindsLocal ctx bs mBody
  in case b of
    NonRec ann ident expr ->
      let (c, esc) = M.findWithDefault (0, False) ident mNext
          ann' = setUsageCount c esc ann
          (expr', mExpr) = computeUsageExpr esc expr
          mFree = mergeUsages mExpr (M.delete ident mNext)
      in (NonRec ann' ident expr' : bs', mFree)
    Rec recBinds ->
      let idents = map (snd . fst) recBinds
          evalExprs = map (\((a, ident), e) -> 
                             let (e', mE) = computeUsageExpr True e 
                             in ((a, ident), e', mE)
                          ) recBinds
          
          mRecExprs = mergeUsagesList (map (\(_,_,m) -> m) evalExprs)
          mRecSaturated = M.map (\(_, esc) -> (-1, esc)) mRecExprs
          totalM = mergeUsages mNext mRecSaturated
          
          recBinds' = map (\((a, ident), e', _) ->
                             let (c, esc) = M.findWithDefault (0, False) ident totalM
                             in ((setUsageCount c esc a, ident), e')
                          ) evalExprs
          
          mFree = foldr M.delete totalM idents
      in (Rec recBinds' : bs', mFree)

computeUsageExpr :: Bool -> Expr Ann -> (Expr Ann, UsageMap)
computeUsageExpr ctx (Literal ann lit) =
  let (lit', m) = case lit of
        NumericLiteral n -> (NumericLiteral n, M.empty)
        StringLiteral s -> (StringLiteral s, M.empty)
        CharLiteral c -> (CharLiteral c, M.empty)
        BooleanLiteral bool -> (BooleanLiteral bool, M.empty)
        ArrayLiteral arr ->
          let (arr', ms) = unzip $ map (computeUsageExpr ctx) arr
          in (ArrayLiteral arr', mergeUsagesList ms)
        ObjectLiteral obj ->
          let (obj', ms) = unzip $ map (\(k, e) -> let (e', mE) = computeUsageExpr ctx e in ((k, e'), mE)) obj
          in (ObjectLiteral obj', mergeUsagesList ms)
  in (Literal ann lit', m)

computeUsageExpr _ (Constructor ann ty ctor fields) =
  (Constructor ann ty ctor fields, M.empty)

computeUsageExpr ctx (Accessor ann field expr) =
  let (expr', m) = computeUsageExpr ctx expr
  in (Accessor ann field expr', m)

computeUsageExpr ctx (ObjectUpdate ann expr keys updates) =
  let (expr', m1) = computeUsageExpr ctx expr
      (updates', ms) = unzip $ map (\(k, e) -> let (e', m) = computeUsageExpr ctx e in ((k, e'), m)) updates
  in (ObjectUpdate ann expr' keys updates', mergeUsages m1 (mergeUsagesList ms))

computeUsageExpr ctx (Abs ann ident body) =
  let (body', mBody) = computeUsageExpr True body
      (count, esc) = M.findWithDefault (0, False) ident mBody
      mFree = M.delete ident mBody
      mFreeCtx = M.map (\(_, _) -> (-1, ctx)) mFree
      ann' = setUsageCount count esc ann
  in (Abs ann' ident body', mFreeCtx)

computeUsageExpr _ (App ann f x) =
  let (f', m1) = computeUsageExpr True f
      (x', m2) = computeUsageExpr True x
  in (App ann f' x', mergeUsages m1 m2)

computeUsageExpr ctx (Var ann q) =
  let m = case q of
            Qualified (BySourcePos _) ident -> M.singleton ident (1, ctx)
            Qualified ByNullSourcePos ident -> M.singleton ident (1, ctx)
            Qualified (ByModuleName _) _ -> M.empty
  in (Var ann q, m)

computeUsageExpr ctx (Case ann exprs alts) =
  let (exprs', m1s) = unzip $ map (computeUsageExpr False) exprs
      (alts', m2s) = unzip $ map (computeUsageAlt ctx) alts
  in (Case ann exprs' alts', mergeUsages (mergeUsagesList m1s) (mergeUsagesListMax m2s))

computeUsageExpr ctx (Let ann binds expr) =
  let (expr', mExpr) = computeUsageExpr ctx expr
      (binds', mBindsAndExpr) = computeUsageBindsLocal ctx binds mExpr
  in (Let ann binds' expr', mBindsAndExpr)

computeUsageExpr ctx (TypeApp ann expr ty) =
  let (expr', m) = computeUsageExpr ctx expr
  in (TypeApp ann expr' ty, m)

computeUsageAlt :: Bool -> CaseAlternative Ann -> (CaseAlternative Ann, UsageMap)
computeUsageAlt ctx (CaseAlternative binders result) =
  let (result', mRes) = case result of
        Right expr -> 
          let (e', m) = computeUsageExpr ctx expr in (Right e', m)
        Left guards -> 
          let (guards', ms) = unzip $ map (\(g, e) -> 
                 let (g', m1) = computeUsageExpr False g
                     (e', m2) = computeUsageExpr ctx e
                 in ((g', e'), mergeUsages m1 m2)
                 ) guards
          in (Left guards', mergeUsagesList ms)
      
      (binders', mFree) = annotateBinders binders mRes
  in (CaseAlternative binders' result', mFree)

annotateBinders :: [Binder Ann] -> UsageMap -> ([Binder Ann], UsageMap)
annotateBinders bs m = 
  let boundNames = concatMap binderNames bs
      annotate b = case b of
        VarBinder ann ident -> 
          let (c, esc) = M.findWithDefault (0, False) ident m
          in VarBinder (setUsageCount c esc ann) ident
        NamedBinder ann ident inner -> 
          let (c, esc) = M.findWithDefault (0, False) ident m
              inner' = annotate inner
          in NamedBinder (setUsageCount c esc ann) ident inner'
        ConstructorBinder ann ty ctor args ->
          ConstructorBinder ann ty ctor (map annotate args)
        NullBinder ann -> NullBinder ann
        LiteralBinder ann lit ->
          let lit' = case lit of
                NumericLiteral n -> NumericLiteral n
                StringLiteral s -> StringLiteral s
                CharLiteral c -> CharLiteral c
                BooleanLiteral bool -> BooleanLiteral bool
                ArrayLiteral arr -> ArrayLiteral (map annotate arr)
                ObjectLiteral obj -> ObjectLiteral (map (\(k, v) -> (k, annotate v)) obj)
          in LiteralBinder ann lit'
      
      bs' = map annotate bs
      mFree = foldr M.delete m boundNames
  in (bs', mFree)

binderNames :: Binder a -> [Ident]
binderNames (VarBinder _ ident) = [ident]
binderNames (NamedBinder _ ident b) = ident : binderNames b
binderNames (ConstructorBinder _ _ _ bs) = concatMap binderNames bs
binderNames (LiteralBinder _ _) = []
binderNames (NullBinder _) = []
