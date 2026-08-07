cat << 'PATCH' > cse.patch
--- src/Language/PureScript/CoreFn/CSE.hs
+++ src/Language/PureScript/CoreFn/CSE.hs
@@ -23,7 +23,7 @@
 import Language.PureScript.CoreFn.Ann (Ann, CoreFnType)
 import Language.PureScript.CoreFn.Binders (Binder(..))
 import Language.PureScript.CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..))
-import Language.PureScript.CoreFn.Meta (Meta)
+import Language.PureScript.CoreFn.Meta (Meta(IsSyntheticApp))
 import Language.PureScript.CoreFn.Traversals (everywhereOnValues, traverseCoreFn)
 import Language.PureScript.Environment (dictTypeName)
 import Language.PureScript.Names (pattern ByNullSourcePos, Ident(..), ModuleName, ProperName(..), Qualified(..), QualifiedBy(..), freshIdent, runIdent, toMaybeModuleName)
@@ -393,8 +393,12 @@
 
   -- This is the one place (I think?) that keeps this from being a general
   -- common subexpression elimination pass.
-  shouldFloatExpr :: Expr a -> Bool
-  shouldFloatExpr _ = False
+  shouldFloatExpr :: Expr Ann -> Bool
+  shouldFloatExpr = \case
+    App (_, _, _, Just IsSyntheticApp) e _ -> isSimple e
+    _                                   -> False
+
+  isSimple :: Expr Ann -> Bool
+  isSimple = \case
+    Var{}          -> True
+    Accessor _ _ e -> isSimple e
+    _              -> False
 
   handleAndWrapExpr :: Expr Ann -> CSEMonad (Expr Ann)
   handleAndWrapExpr = getNewBindsAsLet . handleExpr
PATCH
patch -p0 < cse.patch
stack build --fast && stack install
