import re
import sys

with open("/Users/0x1/Documents/htdocs/purescript/src/Language/PureScript/CoreFn/ToJSON.hs") as f:
    code = f.read()

# Add Control.Monad.State
code = code.replace("import Data.Text qualified as T\n", "import Data.Text qualified as T\nimport Control.Monad.State\n")

state_def = """
type TypeTableState = State (M.Map CoreFnType Int, Int, [(Int, CoreFnType)])

internType :: CoreFnType -> TypeTableState Int
internType ty = do
  (m, nextId, table) <- get
  case M.lookup ty m of
    Just id -> return id
    Nothing -> do
      let id = nextId
      put (M.insert ty id m, nextId + 1, (id, ty) : table)
      return id

runTypeTable :: TypeTableState a -> (a, [Value])
runTypeTable m = 
  let (res, (_, _, table)) = runState m (M.empty, 0, [])
      -- table is accumulated in reverse order
      sortedTable = map snd $ reverse table
  in (res, map coreFnTypeToJSON sortedTable)
"""

code = code.replace("constructorTypeToJSON :: ConstructorType -> Value", state_def + "\nconstructorTypeToJSON :: ConstructorType -> Value")

# Rewrite annToJSON
code = re.sub(
    r"annToJSON :: Ann -> Value\nannToJSON \(ss, _, mty, m\) = object \[.*?\]",
    """annToJSON :: Ann -> TypeTableState Value
annToJSON (ss, _, mty, m) = do
  typeVal <- case mty of
    Nothing -> pure Null
    Just ty -> toJSON <$> internType ty
  pure $ object [ "sourceSpan"  .= sourceSpanToJSON ss
                , "typeId"      .= typeVal
                , "meta"        .= maybe Null metaToJSON m
                ]""",
    code,
    flags=re.DOTALL
)

# Replace signatures
sigs = [
    ("bindToJSON :: Bind Ann -> Value", "bindToJSON :: Bind Ann -> TypeTableState Value"),
    ("exprToJSON :: Expr Ann -> Value", "exprToJSON :: Expr Ann -> TypeTableState Value"),
    ("caseAlternativeToJSON :: CaseAlternative Ann -> Value", "caseAlternativeToJSON :: CaseAlternative Ann -> TypeTableState Value"),
    ("binderToJSON :: Binder Ann -> Value", "binderToJSON :: Binder Ann -> TypeTableState Value"),
    ("literalToJSON :: (a -> Value) -> Literal a -> Value", "literalToJSON :: (a -> TypeTableState Value) -> Literal a -> TypeTableState Value"),
    ("recordToJSON :: (a -> Value) -> [(PSString, a)] -> Value", "recordToJSON :: (a -> TypeTableState Value) -> [(PSString, a)] -> TypeTableState Value")
]
for old, new in sigs:
    code = code.replace(old, new)

# recordToJSON
code = code.replace(
    "recordToJSON f = toJSON . map (toJSON *** f)",
    """recordToJSON f xs = do
  xs' <- mapM (\\(k, v) -> do
    v' <- f v
    pure (toJSON k, v')) xs
  pure $ toJSON xs'"""
)

# Replace `= object` with `= pure $ object` inside the JSON generators
for func in ["literalToJSON", "bindToJSON", "exprToJSON", "caseAlternativeToJSON", "binderToJSON"]:
    code = re.sub(r'(' + func + r'[^\n]+\n\s*)= object', r'\1= pure $ object', code)

# Fix moduleToJSON
code = re.sub(
    r"moduleToJSON :: Version -> Module Ann -> Value\nmoduleToJSON v m = object\n  \[.*?\]",
    """moduleToJSON :: Version -> Module Ann -> Value
moduleToJSON v m =
  let (res, typeTableFinal) = runTypeTable $ do
         decls' <- mapM bindToJSON (moduleDecls m)
         foreignAnns' <- mapM (\\(ann, ident) -> do
            annVal <- annToJSON ann
            pure (T.unpack (Language.PureScript.Names.runIdent ident) .= annVal)
          ) (moduleForeign m)
         pure (decls', foreignAnns')
         
      (declsValFinal, foreignAnnsFinal) = res
  in object
  [ "sourceSpan" .= sourceSpanToJSON (moduleSourceSpan m)
  , "moduleName" .= moduleNameToJSON (moduleName m)
  , "modulePath" .= toJSON (modulePath m)
  , "imports"    .= map importToJSON (moduleImports m)
  , "exports"    .= map identToJSON (moduleExports m)
  , "reExports"  .= reExportsToJSON (moduleReExports m)
  , "foreign"    .= map (identToJSON . snd) (moduleForeign m)
  , "foreignAnnotations" .= object foreignAnnsFinal
  , "decls"      .= declsValFinal
  , "dataDecls"  .= map dataDeclToJSON (moduleDataDecls m)
  , "classDecls" .= map classDeclToJSON (moduleClassDecls m)
  , "builtWith"  .= toJSON (showVersion v)
  , "comments"   .= map toJSON (moduleComments m)
  , "typeTable"  .= toJSON typeTableFinal
  ]""",
    code,
    flags=re.DOTALL
)

with open("/Users/0x1/Documents/htdocs/purescript/scratch/ToJSON_rewritten.hs", "w") as f:
    f.write(code)
