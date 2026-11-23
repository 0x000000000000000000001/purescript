{-# LANGUAGE DoAndIfThenElse #-}

module TestCoreFn (spec) where

import Prelude

import Control.Monad.Supply (runSupply)
import Data.Aeson (Result(..), Value(..))
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (parse)
import Data.Map qualified as M
import Data.Version (Version(..))

import Language.PureScript.AST qualified as A
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (pattern NullSourceAnn, SourcePos(..), SourceSpan(..))
import Language.PureScript.Comments (Comment(..))
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.CoreFn (Ann, Bind(..), Binder(..), CaseAlternative(..), ConstructorType(..), Expr(..), Meta(..), Module(..), ssAnn, CoreFnType(..))
import Language.PureScript.CoreFn.Ann (BindingId(..), BindingUsage(..), LastLocalUse(..), UsageInfo(..), VariableUse(..), emptyUsageInfo)
import Language.PureScript.CoreFn.Desugar (moduleToCoreFn)
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Language.PureScript.CoreFn.Module (ClassDecl(..), DataConstructor(..), DataDecl(..))
import Language.PureScript.CoreFn.Optimizer (optimizeCoreFn)
import Language.PureScript.CoreFn.ToJSON (moduleToJSON)
import Language.PureScript.CoreFn.Usage (computeUsage)
import Language.PureScript.Environment (DataDeclType(..), Environment(..), NameKind(Public), TypeKind(..), initEnvironment, kindType, (-:>))
import Language.PureScript.Errors (MultipleErrors, errorCode, runMultipleErrors)
import Language.PureScript.Label (Label(..))
import Language.PureScript.Names (pattern ByNullSourcePos, Ident(..), ModuleName(..), ProperName(..), Qualified(..), QualifiedBy(..))
import Language.PureScript.PSString (mkString)
import Language.PureScript.Roles (Role(..))
import Language.PureScript.TypeChecker.Skolems (skolemEscapeCheck)
import Language.PureScript.Types qualified as T

import Test.Hspec (Spec, context, expectationFailure, shouldBe, shouldSatisfy, specify)

parseModule :: Value -> Result (Version, Module Ann)
parseModule = parse moduleFromJSON

-- convert a module to its json CoreFn representation and back
parseMod :: ([DataDecl] -> [ClassDecl] -> Module Ann) -> Result (Module Ann)
parseMod mkM =
  let m = mkM [] []
      v = Version [0] []
  in snd <$> parseModule (moduleToJSON v m)

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess _           = False

mapJsonObjects :: (KM.KeyMap Value -> KM.KeyMap Value) -> Value -> Value
mapJsonObjects change = go
  where
  go (Object fields) = Object $ change $ fmap go fields
  go (Array values) = Array $ fmap go values
  go value = value

spec :: Spec
spec = context "CoreFnFromJson" $ do
  let mn = ModuleName "Example.Main"
      mp = "src/Example/Main.purs"
      ss = SourceSpan mp (SourcePos 0 0) (SourcePos 0 0)
      ann = ssAnn ss

  context "typed function application optimization" $ do
    let qualified owner name = Qualified (ByModuleName $ ModuleName owner) (Ident name)
        typed ty = (ss, [], Just ty, Nothing, Nothing)
        fnType = CFFunc [CFInt] CFString
        fn = Var (typed fnType) (qualified "Functions" "render")
        arg = Literal (typed CFInt) (NumericLiteral $ Left 42)
        resultAnn = (ss, [LineComment "application result"], Just CFString, Nothing, Nothing)
        helper name = Var ann (qualified "Data.Function" name)
        instantiated name = TypeApp ann (TypeApp ann (helper name) CFInt) CFString
        call helperExpr first second = App resultAnn (App ann helperExpr first) second
        fixture expr = Module ss [] mn mp [] [] M.empty []
          [NonRec ann (Ident "test") expr] [] []
        optimize = fst . runSupply 0 . optimizeCoreFn
        declarations expr = moduleDecls $ optimize $ fixture expr
        expected expr = [NonRec ann (Ident "test") expr]

    specify "still simplifies apply and applyFlipped without type applications" $ do
      declarations (call (helper "apply") fn arg) `shouldBe` expected (App resultAnn fn arg)
      declarations (call (helper "applyFlipped") arg fn) `shouldBe` expected (App resultAnn fn arg)

    specify "simplifies apply through nested type applications" $
      declarations (call (instantiated "apply") fn arg) `shouldBe` expected (App resultAnn fn arg)

    specify "simplifies applyFlipped through nested type applications" $
      declarations (call (instantiated "applyFlipped") arg fn) `shouldBe` expected (App resultAnn fn arg)

    specify "preserves operand instantiations and the complete result annotation" $ do
      let polyFn = Var (typed $ CFForAll ["a"] $ CFFunc [CFTypeVar "a"] CFString)
            (qualified "Functions" "renderAny")
          typedFn = TypeApp (typed fnType) polyFn CFInt
          polyArg = Var (typed $ CFForAll ["a"] $ CFTypeVar "a")
            (qualified "Functions" "value")
          typedArg = TypeApp (typed CFInt) polyArg CFInt
      declarations (call (instantiated "apply") typedFn typedArg)
        `shouldBe` expected (App resultAnn typedFn typedArg)
      declarations (call (instantiated "applyFlipped") typedArg typedFn)
        `shouldBe` expected (App resultAnn typedFn typedArg)

    specify "preserves type applications surrounding the complete expression" $ do
      let polymorphic = CFForAll ["a"] $ CFFunc [CFTypeVar "a"] (CFTypeVar "a")
          maker = Var (typed $ CFFunc [CFInt] polymorphic) (qualified "Functions" "makeIdentity")
          apply = TypeApp ann (TypeApp ann (helper "apply") CFInt) polymorphic
          outer expr = TypeApp (typed $ CFFunc [CFString] CFString) expr CFString
          input = outer $ App (typed polymorphic) (App ann apply maker) arg
          output = outer $ App (typed polymorphic) maker arg
      declarations input `shouldBe` expected output

    specify "preserves partial applications and unrelated functions with the same name" $ do
      let partial = App (typed fnType) (instantiated "apply") fn
          other = TypeApp ann (Var ann $ qualified "Application" "apply") CFInt
          unrecognized = call other fn arg
      mapM_ (\expr -> declarations expr `shouldBe` expected expr)
        [partial, unrecognized]

    specify "keeps typed module declarations and the optimized expression in serialized TAST" $ do
      let original = (fixture $ call (instantiated "apply") fn arg)
            { moduleDataDecls = [DataDecl (ProperName "Box") ["a"]
                [DataConstructor (ProperName "Box") [CFTypeVar "a"]]]
            , moduleClassDecls = [ClassDecl (ProperName "Render") ["a"] []
                [(Ident "render", CFFunc [CFTypeVar "a"] CFString)]]
            , moduleForeign = [(typed fnType, Ident "foreignRender")]
            }
          optimized = optimize original
          wanted = original { moduleDecls = expected (App resultAnn fn arg) }
          version = Version [0] []
      moduleToJSON version optimized `shouldBe` moduleToJSON version wanted
      case parseModule $ moduleToJSON version optimized of
        Error message -> expectationFailure message
        Success (_, decoded) -> moduleToJSON version decoded `shouldBe` moduleToJSON version wanted

  context "local usage facts" $ do
    let x = Ident "x"
        y = Ident "y"
        f = Ident "f"
        local ident = Var ann (Qualified ByNullSourcePos ident)
        global ident = Var ann (Qualified (ByModuleName mn) ident)
        values = Literal ann . ArrayLiteral
        unit = Literal ann (BooleanLiteral True)
        usage (_, _, _, _, value) = value
        annotate expr =
          let [NonRec _ _ result] = moduleDecls $ computeUsage $
                Module ss [] mn mp [] [] M.empty [] [NonRec ann (Ident "test") expr] [] []
          in fmap usage result
        bound value = value >>= bindingUsage
        occurrence value = value >>= variableUse
        identity value = bindingUsageId <$> bound value
        occurrenceIdentity value = variableBindingId <$> occurrence value
        count value = bindingMaxUses <$> bound value
        escaping value = bindingEscapingContext <$> bound value
        lastUse value = variableLastLocalUse <$> occurrence value

    specify "distinguishes an unused binding from a parameter used exactly once" $ do
      let Let _ [NonRec unused _ _] _ = annotate $ Let ann [NonRec ann x unit] unit
          Abs once _ (Var use _) = annotate $ Abs ann y (local y)
      count unused `shouldBe` Just (Just 0)
      escaping unused `shouldBe` Just (Just False)
      count once `shouldBe` Just (Just 1)
      occurrenceIdentity use `shouldBe` identity once
      lastUse use `shouldBe` Just ProvenLastLocalUse

    specify "counts uses per parameter instance even when the function is called twice" $ do
      let expr = Let ann [NonRec ann f $ Abs ann x $ values [local x, local x]] $
            values [App ann (local f) unit, App ann (local f) unit]
          Let _ [NonRec fAnn _ (Abs xAnn _ (Literal _ (ArrayLiteral [Var first _, Var final _])))] _ = annotate expr
      count fAnn `shouldBe` Just (Just 2)
      count xAnn `shouldBe` Just (Just 2)
      occurrenceIdentity first `shouldBe` identity xAnn
      occurrenceIdentity final `shouldBe` identity xAnn
      lastUse first `shouldBe` Just UnknownLastLocalUse
      lastUse final `shouldBe` Just ProvenLastLocalUse

    specify "adds sequential uses and links a local let binding to its occurrences" $ do
      let expr = Let ann [NonRec ann x unit] $ values [local x, local x, local x]
          Let _ [NonRec xAnn _ _] (Literal _ (ArrayLiteral [Var first _, Var second _, Var final _])) = annotate expr
      count xAnn `shouldBe` Just (Just 3)
      map occurrenceIdentity [first, second, final] `shouldBe` replicate 3 (identity xAnn)
      map lastUse [first, second, final]
        `shouldBe` [Just UnknownLastLocalUse, Just UnknownLastLocalUse, Just ProvenLastLocalUse]

    specify "takes the maximum across exclusive branches and proves each branch's final use" $ do
      let expr = Abs ann x $ Case ann [unit]
            [ CaseAlternative [NullBinder ann] $ Right $ values [local x, local x]
            , CaseAlternative [NullBinder ann] $ Right $ local x
            ]
          Abs xAnn _ (Case _ _
            [ CaseAlternative _ (Right (Literal _ (ArrayLiteral [Var first _, Var final _])))
            , CaseAlternative _ (Right (Var other _))
            ]) = annotate expr
      count xAnn `shouldBe` Just (Just 2)
      map occurrenceIdentity [first, final, other] `shouldBe` replicate 3 (identity xAnn)
      map lastUse [first, final, other]
        `shouldBe` [Just UnknownLastLocalUse, Just ProvenLastLocalUse, Just ProvenLastLocalUse]

    specify "keeps guard failure live through a later alternative" $ do
      let expr = Abs ann x $ Case ann [unit]
            [ CaseAlternative [NullBinder ann] $ Left [(local x, unit)]
            , CaseAlternative [NullBinder ann] $ Right $ local x
            ]
          Abs xAnn _ (Case _ _
            [ CaseAlternative _ (Left [(Var guardUse _, _)])
            , CaseAlternative _ (Right (Var fallback _))
            ]) = annotate expr
      count xAnn `shouldBe` Just (Just 2)
      lastUse guardUse `shouldBe` Just UnknownLastLocalUse
      lastUse fallback `shouldBe` Just ProvenLastLocalUse

    specify "keeps a failed guard live through the next guard in the same alternative" $ do
      let falseGuard = Case ann [local x]
            [CaseAlternative [NullBinder ann] $ Right $ Literal ann (BooleanLiteral False)]
          expr = Abs ann x $ Case ann [unit]
            [CaseAlternative [NullBinder ann] $ Left [(falseGuard, unit), (local x, local x)]]
          Abs xAnn _ (Case _ _ [CaseAlternative _ (Left
            [ (Case _ [Var firstGuard _] _, _)
            , (Var secondGuard _, Var success _)
            ])]) = annotate expr
      count xAnn `shouldBe` Just (Just 3)
      map lastUse [firstGuard, secondGuard, success]
        `shouldBe` [Just UnknownLastLocalUse, Just UnknownLastLocalUse, Just ProvenLastLocalUse]

    specify "does not mark branch uses as last when another use follows the case" $ do
      let branch = Case ann [unit]
            [ CaseAlternative [NullBinder ann] $ Right $ local x
            , CaseAlternative [NullBinder ann] $ Right $ local x
            ]
          Abs xAnn _ (Literal _ (ArrayLiteral
            [ Case _ _ [CaseAlternative _ (Right (Var left _)), CaseAlternative _ (Right (Var right _))]
            , Var final _
            ])) = annotate $ Abs ann x $ values [branch, local x]
      count xAnn `shouldBe` Just (Just 2)
      map lastUse [left, right, final]
        `shouldBe` [Just UnknownLastLocalUse, Just UnknownLastLocalUse, Just ProvenLastLocalUse]

    specify "keeps every occurrence of a reusable closure capture unknown" $ do
      let Abs xAnn _ (Literal _ (ArrayLiteral [Abs yAnn _ (Var captured _), Var later _])) =
            annotate $ Abs ann x $ values [Abs ann y (local x), local x]
      count xAnn `shouldBe` Just Nothing
      count yAnn `shouldBe` Just (Just 0)
      map occurrenceIdentity [captured, later] `shouldBe` replicate 2 (identity xAnn)
      map lastUse [captured, later] `shouldBe` replicate 2 (Just UnknownLastLocalUse)

    specify "preserves escaping uses inside a closure evaluated as a case scrutinee" $ do
      let expr = Abs ann x $ Case ann [Abs ann y $ App ann (global f) (local x)]
            [CaseAlternative [NullBinder ann] $ Right unit]
          Abs xAnn _ (Case _ [Abs _ _ (App _ _ (Var captured _))] _) = annotate expr
      count xAnn `shouldBe` Just Nothing
      escaping xAnn `shouldBe` Just (Just True)
      occurrenceIdentity captured `shouldBe` identity xAnn
      lastUse captured `shouldBe` Just UnknownLastLocalUse

    specify "assigns distinct identities to shadowed bindings and restores the outer scope" $ do
      let Abs outer _ (Literal _ (ArrayLiteral
            [ Var first _
            , Let _ [NonRec inner _ _] (Var innerUse _)
            , Var final _
            ])) = annotate $ Abs ann x $ values [local x, Let ann [NonRec ann x unit] (local x), local x]
      count outer `shouldBe` Just (Just 2)
      count inner `shouldBe` Just (Just 1)
      identity inner `shouldSatisfy` (/= identity outer)
      map occurrenceIdentity [first, final] `shouldBe` replicate 2 (identity outer)
      occurrenceIdentity innerUse `shouldBe` identity inner
      map lastUse [first, innerUse, final]
        `shouldBe` [Just UnknownLastLocalUse, Just ProvenLastLocalUse, Just ProvenLastLocalUse]

    specify "tracks variables bound inside nested object and array patterns" $ do
      let patternBinder = LiteralBinder ann $ ObjectLiteral
            [(mkString "items", LiteralBinder ann $ ArrayLiteral [VarBinder ann x])]
          Case _ _ [CaseAlternative
            [LiteralBinder _ (ObjectLiteral [(_, LiteralBinder _ (ArrayLiteral [VarBinder xAnn _]))])]
            (Right (Var use _))] = annotate $
              Case ann [global y] [CaseAlternative [patternBinder] (Right $ local x)]
      count xAnn `shouldBe` Just (Just 1)
      occurrenceIdentity use `shouldBe` identity xAnn
      lastUse use `shouldBe` Just ProvenLastLocalUse

    specify "gives a named pattern and its inner alias distinct lexical identities" $ do
      let Case _ _ [CaseAlternative [NamedBinder namedAnn _ (VarBinder innerAnn _)]
            (Right (Literal _ (ArrayLiteral [Var namedUse _, Var innerUse _])))] = annotate $
              Case ann [global f] [CaseAlternative [NamedBinder ann x (VarBinder ann y)] $
                Right $ values [local x, local y]]
      count namedAnn `shouldBe` Just (Just 1)
      count innerAnn `shouldBe` Just (Just 1)
      identity namedAnn `shouldSatisfy` (/= identity innerAnn)
      occurrenceIdentity namedUse `shouldBe` identity namedAnn
      occurrenceIdentity innerUse `shouldBe` identity innerAnn
      map lastUse [namedUse, innerUse] `shouldBe` replicate 2 (Just ProvenLastLocalUse)

    specify "recomputes usage idempotently and clears stale annotations before analysis" $ do
      let fixture = Module ss [] mn mp [(ann, mn)] [] M.empty [(ann, y)]
            [NonRec ann f $ Abs ann x $ values [local x, local x]] [] []
          analyzed = computeUsage fixture
          stale (span_, comments, ty, meta, _) = (span_, comments, ty, meta, Just $ UsageInfo
            (Just $ BindingUsage (BindingId 999) (Just 999) (Just False))
            (Just $ VariableUse (BindingId 999) ProvenLastLocalUse))
          relevant result = (moduleImports result, moduleForeign result, moduleDecls result)
      relevant (computeUsage analyzed) `shouldBe` relevant analyzed
      relevant (computeUsage $ fmap stale analyzed) `shouldBe` relevant analyzed

    specify "keeps local recursive binding multiplicity and final uses conservative" $ do
      let Let _ [Rec [((fAnn, _), Abs _ _ (App _ (Var recursive _) _))]] (Var final _) =
            annotate $ Let ann [Rec [((ann, f), Abs ann y $ App ann (local f) (local y))]] (local f)
      count fAnn `shouldBe` Just Nothing
      map occurrenceIdentity [recursive, final] `shouldBe` replicate 2 (identity fAnn)
      map lastUse [recursive, final] `shouldBe` replicate 2 (Just UnknownLastLocalUse)

    specify "does not attach local identity facts to global declarations or qualified references" $ do
      let result = computeUsage $ Module ss [] mn mp [(ann, mn)] [] M.empty [(ann, y)]
            [NonRec ann x (global x)] [] []
          [NonRec declaration _ (Var reference _)] = moduleDecls result
          annotations = [declaration, reference] <> map fst (moduleImports result) <> map fst (moduleForeign result)
      map (bound . usage) annotations `shouldBe` replicate 4 Nothing
      map (occurrence . usage) annotations `shouldBe` replicate 4 Nothing

    specify "does not count a module-qualified name as a use of a same-named local" $ do
      let Abs xAnn _ (Literal _ (ArrayLiteral [Var qualified _, Var direct _])) =
            annotate $ Abs ann x $ values [global x, local x]
      count xAnn `shouldBe` Just (Just 1)
      occurrence qualified `shouldBe` Nothing
      occurrenceIdentity direct `shouldBe` identity xAnn
      lastUse direct `shouldBe` Just ProvenLastLocalUse

    specify "does not attach usage facts to applications or constructors" $ do
      let constructor = Constructor ann (ProperName "Tree") (ProperName "Node") []
      annotate (App ann (global x) constructor) `shouldBe`
        App Nothing (Var Nothing $ Qualified (ByModuleName mn) x)
          (Constructor Nothing (ProperName "Tree") (ProperName "Node") [])

    specify "describes a final local alias use without claiming the referenced object is unique" $ do
      let Abs xAnn _ (Let _ [NonRec yAnn _ (Var aliasSource _)] (Var returned _)) =
            annotate $ Abs ann x $ Let ann [NonRec ann y (local x)] (local y)
      count xAnn `shouldBe` Just (Just 1)
      count yAnn `shouldBe` Just (Just 1)
      identity xAnn `shouldSatisfy` (/= identity yAnn)
      occurrenceIdentity aliasSource `shouldBe` identity xAnn
      occurrenceIdentity returned `shouldBe` identity yAnn
      map lastUse [aliasSource, returned] `shouldBe` replicate 2 (Just ProvenLastLocalUse)

    specify "separates a non-escaping scrutinee use from a returned child field" $ do
      let ctor = ConstructorBinder ann (Qualified (ByModuleName mn) $ ProperName "Tree")
            (Qualified (ByModuleName mn) $ ProperName "Node") [VarBinder ann y]
          Abs treeAnn _ (Case _ [Var scrutinee _]
            [CaseAlternative [ConstructorBinder _ _ _ [VarBinder childAnn _]] (Right (Var child _))]) =
              annotate $ Abs ann x $ Case ann [local x] [CaseAlternative [ctor] (Right $ local y)]
      count treeAnn `shouldBe` Just (Just 1)
      escaping treeAnn `shouldBe` Just (Just False)
      escaping childAnn `shouldBe` Just (Just True)
      occurrenceIdentity scrutinee `shouldBe` identity treeAnn
      occurrenceIdentity child `shouldBe` identity childAnn
      identity treeAnn `shouldSatisfy` (/= identity childAnn)

  context "usage JSON contract" $ do
    let x = Ident "x"
        local = Var ann (Qualified ByNullSourcePos x)
        fixture = computeUsage $ Module ss [] mn mp [] [] M.empty []
          [NonRec ann (Ident "test") (Abs ann x local)] [] []
        encoded = moduleToJSON (Version [0] []) fixture
        changeObject change (Object fields) = Object (change fields)
        changeObject _ value = value
        changeBlock name change = mapJsonObjects $ KM.mapWithKey $ \key value ->
          if key == name then changeObject change value else value
        clearUsage (span_, comments, ty, meta, _) = (span_, comments, ty, meta, Nothing)
        containsKey key (Object fields) = KM.member key fields || any (containsKey key) fields
        containsKey key (Array values) = any (containsKey key) values
        containsKey _ _ = False
        forgetFacts (span_, comments, ty, meta, info) =
          (span_, comments, ty, meta, fmap (\value -> value
            { bindingUsage = fmap (\binding -> binding
                { bindingMaxUses = Nothing, bindingEscapingContext = Nothing }) (bindingUsage value)
            , variableUse = fmap (\use -> use
                { variableLastLocalUse = UnknownLastLocalUse }) (variableUse value)
            }) info)
        expectDeclarations json expected = do
          let result = moduleDecls . snd <$> parseModule json
          result `shouldSatisfy` isSuccess
          case result of
            Error _ -> pure ()
            Success actual -> actual `shouldBe` expected
        rejects json = parseModule json `shouldSatisfy` (not . isSuccess)

    specify "round-trips direct usage facts without historical fields or a root marker" $ do
      map (`containsKey` encoded) ["usageCount", "escapes", "usageAnalysis", "tastVersion"]
        `shouldBe` replicate 4 False
      expectDeclarations encoded (moduleDecls fixture)

    specify "preserves usage facts across typed annotations and explicit type applications" $ do
      let nestedType = CFFunc [CFInt] (CFRecord (CFRow [(mkString "value", CFInt)] Nothing))
          typedAnn = (ss, [], Just nestedType, Nothing, Nothing)
          typed = computeUsage $ Module ss [] mn mp [] [] M.empty []
            [NonRec ann (Ident "test") $ Abs ann x $ TypeApp typedAnn local CFInt] [] []
      expectDeclarations (moduleToJSON (Version [0] []) typed) (moduleDecls typed)

    specify "treats absent optional count, escape context, and last-use fields as unknown" $ do
      let withoutFacts = changeBlock "variableUse" (KM.delete "lastLocalUse") $
            changeBlock "bindingUsage" (KM.delete "maxUses" . KM.delete "hasEscapingUseContext") encoded
      expectDeclarations withoutFacts (map (fmap forgetFacts) $ moduleDecls fixture)

    specify "round-trips explicit null facts as unknown" $ do
      let unknown = changeBlock "variableUse" (KM.insert "lastLocalUse" Null) $
            changeBlock "bindingUsage" (KM.insert "maxUses" Null . KM.insert "hasEscapingUseContext" Null) encoded
      expectDeclarations unknown (map (fmap forgetFacts) $ moduleDecls fixture)

    specify "treats absent usage blocks as unknown" $ do
      let withoutBlocks = mapJsonObjects (KM.delete "bindingUsage" . KM.delete "variableUse") encoded
      expectDeclarations withoutBlocks (map (fmap clearUsage) $ moduleDecls fixture)

    specify "ignores historical fields without synthesizing usage facts" $ do
      let withoutBlocks = mapJsonObjects (KM.delete "bindingUsage" . KM.delete "variableUse") encoded
          historical = changeBlock "annotation"
            (KM.insert "usageCount" (Number 1) . KM.insert "escapes" (Bool False)) withoutBlocks
      expectDeclarations historical (map (fmap clearUsage) $ moduleDecls fixture)

    specify "does not decode obsolete fields or let them override direct facts" $ do
      let historical = changeBlock "annotation"
            (KM.insert "usageCount" (String "ignored") . KM.insert "escapes" (Number (-1))) encoded
      expectDeclarations historical (moduleDecls fixture)

    specify "rejects negative or fractional usage counts and binding identities" $ do
      mapM_ (\count -> rejects $ changeBlock "bindingUsage" (KM.insert "maxUses" count) encoded)
        [Number (-1), Number 0.5]
      mapM_ (\name -> rejects $ changeBlock name (KM.insert "bindingId" $ Number (-1)) encoded)
        ["bindingUsage", "variableUse"]
      rejects $ changeBlock "bindingUsage" (KM.insert "bindingId" $ Number 0.5) encoded

    specify "preserves usage counts larger than machine-sized integers" $ do
      let huge = toInteger (maxBound :: Int) + 1
          withHugeCount = changeBlock "bindingUsage" (KM.insert "maxUses" $ Number $ fromInteger huge) encoded
          update (span_, comments, ty, meta, info) =
            (span_, comments, ty, meta, fmap (\value -> value
              { bindingUsage = fmap (\binding -> binding { bindingMaxUses = Just huge }) (bindingUsage value) }) info)
      expectDeclarations withHugeCount (map (fmap update) $ moduleDecls fixture)

    specify "rejects a false last-use proof and a non-boolean escaping context" $ do
      rejects $ changeBlock "variableUse" (KM.insert "lastLocalUse" $ Bool False) encoded
      rejects $ changeBlock "bindingUsage" (KM.insert "hasEscapingUseContext" $ Number 0) encoded

    specify "rejects a variable identity with no matching local binding" $ do
      rejects $ changeBlock "variableUse" (KM.insert "bindingId" $ Number 2147483647) encoded

    specify "does not resolve an outer identity through a shadowing binding without facts" $ do
      let shadowOuterId = BindingId 7
          outerAnn = (ss, [], Nothing, Nothing, Just (emptyUsageInfo
            { bindingUsage = Just $ BindingUsage shadowOuterId (Just 0) (Just False) }))
          wronglyLinked = (ss, [], Nothing, Nothing, Just (emptyUsageInfo
            { variableUse = Just $ VariableUse shadowOuterId ProvenLastLocalUse }))
          shadowed = Module ss [] mn mp [] [] M.empty []
            [NonRec ann (Ident "test") $ Abs outerAnn x $ Abs ann x $
              Var wronglyLinked (Qualified ByNullSourcePos x)] [] []
      rejects $ moduleToJSON (Version [0] []) shadowed

    specify "rejects duplicate binding identities and facts on global declarations" $ do
      let binding = Just (emptyUsageInfo
            { bindingUsage = Just $ BindingUsage (BindingId 7) (Just 1) (Just False) })
          bindingAnn = (ss, [], Nothing, Nothing, binding)
          duplicate = Module ss [] mn mp [] [] M.empty []
            [NonRec ann x $ Abs bindingAnn x $ Abs bindingAnn (Ident "y") $ Literal ann (BooleanLiteral True)] [] []
          global = Module ss [] mn mp [] [] M.empty []
            [NonRec bindingAnn x $ Literal ann (BooleanLiteral True)] [] []
      rejects $ moduleToJSON (Version [0] []) duplicate
      rejects $ moduleToJSON (Version [0] []) global

  context "typed declaration JSON" $ do
    let version = Version [0] []
        variable = CFTypeVar "a"
        item = CFAdt (Qualified (ByModuleName mn) (ProperName "Item")) [CFInt]
        superclass = Qualified (ByModuleName mn) (ProperName "Show")
        dataDecl = DataDecl (ProperName "Box") ["a"]
          [DataConstructor (ProperName "Box") [variable, CFArray item]]
        classDecl = ClassDecl (ProperName "Render") ["a"] [(superclass, [variable])]
          [(Ident "render", CFConstrainedType [(superclass, [variable])] (CFFunc [variable] CFString))]
        fixture = Module ss [] mn mp [] [] M.empty [] [] [dataDecl] [classDecl]
        encoded = moduleToJSON version fixture
        rejects json = parseModule json `shouldSatisfy` (not . isSuccess)
        changeType tag change = mapJsonObjects $ \fields ->
          if KM.lookup "type" fields == Just (String tag) then change fields else fields
        changeField name change = KM.mapWithKey $ \key value ->
          if key == name then change value else value
        roundTrips json = case parseModule json of
          Error err -> expectationFailure err
          Success (_, parsed) -> moduleToJSON version parsed `shouldBe` encoded

    specify "round-trips declaration layouts and shared type-table references" $
      roundTrips encoded

    specify "reads the declaration field aliases emitted alongside canonical fields" $ do
      let aliases = mapJsonObjects $ \fields ->
            if KM.member "typeName" fields
              then KM.delete "name" $ KM.delete "vars" fields
              else if KM.member "constructorName" fields
                then KM.delete "name" $ KM.delete "fields" fields
                else fields
      roundTrips (aliases encoded)

    specify "accepts older modules without declaration metadata" $ do
      let empty = Module ss [] mn mp [] [] M.empty [] [] [] []
          legacy = case moduleToJSON version empty of
            Object fields -> Object $ KM.delete "dataDecls" $ KM.delete "classDecls" fields
            value -> value
      case parseModule legacy of
        Error err -> expectationFailure err
        Success (_, parsed) -> moduleToJSON version parsed `shouldBe` moduleToJSON version empty

    specify "rejects empty qualified names in type and superclass metadata" $ do
      let clearName fields = KM.insert "fqn" (Array mempty) fields
      rejects $ changeType "Adt" clearName encoded
      rejects $ mapJsonObjects (\fields ->
        if KM.member "superclasses" fields
          then changeField "superclasses" (mapJsonObjects clearName) fields
          else fields) encoded
      rejects $ changeType "ConstrainedType"
        (changeField "constraints" (mapJsonObjects clearName)) encoded

    specify "rejects unknown type tags instead of silently erasing their types" $
      rejects $ changeType "Int" (KM.insert "type" $ String "UnknownType") encoded

    specify "rejects an array with a missing or malformed element type" $
      mapM_ (\value -> rejects $ changeType "Array" (KM.insert "element" value) encoded)
        [Null, Bool True, String "Int"]

  context "qualified primitive types" $ do
    let a = NullSourceAnn
        int = T.TypeConstructor a C.Int
        custom name = Qualified (ByModuleName (ModuleName "Application")) (ProperName name)
        environment = initEnvironment
          { types = M.union (M.fromList
              [ (custom "Int", (kindType, ExternData []))
              , (custom "Array", (kindType -:> kindType, ExternData [Representational]))
              , (custom "Unit", (kindType, ExternData []))
              ]) (types initEnvironment)
          }
        desugar ty =
          [ annotationType
          | ((_, _, annotationType, _, _), _) <- moduleForeign $ moduleToCoreFn environment $
              A.Module ss [] mn [A.ExternDeclaration (ss, []) (Ident "value") ty] (Just [])
          ]

    specify "distinguishes primitive types from same-named user types" $ do
      desugar int `shouldBe` [Just CFInt]
      desugar (T.TypeConstructor a $ custom "Int") `shouldBe` [Just (CFAdt (custom "Int") [])]
      desugar (T.TypeApp a (T.TypeConstructor a C.Array) int) `shouldBe` [Just (CFArray CFInt)]
      desugar (T.TypeApp a (T.TypeConstructor a $ custom "Array") int)
        `shouldBe` [Just (CFAdt (custom "Array") [CFInt])]
      desugar (T.TypeConstructor a $ Qualified (ByModuleName $ ModuleName "Data.Unit") $ ProperName "Unit")
        `shouldBe` [Just CFUnit]
      desugar (T.TypeConstructor a $ custom "Unit") `shouldBe` [Just (CFAdt (custom "Unit") [])]

  specify "substitutes types in nested binders as well as expression annotations" $ do
    let unknown = T.TUnknown NullSourceAnn 0
        int = T.TypeConstructor NullSourceAnn C.Int
        binder = A.TypedBinder unknown $ A.NamedBinder ss (Ident "outer") $
          A.TypedBinder unknown $ A.VarBinder ss (Ident "inner")
        expr = A.Abs binder $ A.TypedValue True (A.Literal ss $ NumericLiteral $ Left 1) unknown
    case A.overTypes (const int) expr of
      A.Abs (A.TypedBinder outer (A.NamedBinder _ _ (A.TypedBinder inner _))) (A.TypedValue _ _ result) ->
        [outer, inner, result] `shouldBe` replicate 3 int
      other -> expectationFailure $ "Unexpected expression after type substitution: " <> show other

  context "skolem scopes in elaborated expressions" $ do
    let int = T.TypeConstructor NullSourceAnn C.Int
        escaped = T.Skolem NullSourceAnn "a" Nothing 0 (T.SkolemScope 0)
        literal = A.Literal ss (NumericLiteral (Left 1))
        errors expr = case (skolemEscapeCheck (A.TypedValue True expr int) :: Either MultipleErrors ()) of
          Left errs -> map errorCode (runMultipleErrors errs)
          Right () -> []

    specify "checks ordinary unchecked annotations nested inside a checked expression" $
      errors (A.TypedValue False literal escaped) `shouldBe` ["EscapedSkolem"]

    specify "does not reject an intermediate instantiation annotation on its own" $
      errors (A.TypedValue False (A.VisibleTypeApp literal int) escaped) `shouldBe` []

    specify "still checks descendants of an intermediate instantiation" $
      errors (A.TypedValue False (A.VisibleTypeApp (A.TypedValue True literal escaped) int) escaped)
        `shouldBe` ["EscapedSkolem"]

  context "minimum Int desugaring" $ do
    let int = T.TypeConstructor NullSourceAnn C.Int
        qualified owner name = Qualified (ByModuleName $ ModuleName owner) (Ident name)
        negateInt = qualified "Data.Ring" "negate"
        ringInt = qualified "Data.Ring" "ringInt"
        argument = NumericLiteral (Left 2147483648)
        application fn dict = A.App
          (A.App (A.VisibleTypeApp (A.Var ss fn) int) (A.Var ss dict))
          (A.Literal ss argument)
        desugar expression =
          [ fmap (const ()) value
          | NonRec _ _ value <- moduleDecls $ moduleToCoreFn initEnvironment $
              A.Module ss [] mn
                [A.ValueDecl (ss, []) (Ident "value") Public []
                  [A.MkUnguarded $ A.TypedValue True expression int]] (Just [])
          ]
        unchanged fn dict = App () (App () (TypeApp () (Var () fn) CFInt) (Var () dict))
          (Literal () argument)

    specify "folds the built-in Int negation through type applications" $
      desugar (application negateInt ringInt) `shouldBe` [Literal () $ NumericLiteral $ Left (-2147483648)]

    specify "preserves a custom negate function or Ring dictionary" $ do
      let customNegate = qualified "Application" "negate"
          customRing = qualified "Application" "ringInt"
      desugar (application customNegate ringInt) `shouldBe` [unchanged customNegate ringInt]
      desugar (application negateInt customRing) `shouldBe` [unchanged negateInt customRing]

  context "typed row desugaring" $ do
    let a = NullSourceAnn
        int = T.TypeConstructor a C.Int
        kind = T.TypeConstructor a C.Type
        emptyRow = T.REmpty a
        kindedEmpty = T.KindApp a emptyRow kind
        field label ty = T.RCons a (Label label) ty
        record = T.TypeApp a (T.TypeConstructor a C.Record)
        desugar ty =
          [ annotationType
          | ((_, _, annotationType, _, _), _) <- moduleForeign $ moduleToCoreFn initEnvironment $
              A.Module ss [] mn [A.ExternDeclaration (ss, []) (Ident "value") ty] (Just [])
          ]

    specify "preserves closed kind-applied rows and field order" $ do
      let row = field "z" int $ field "a" int kindedEmpty
          expected = CFRow [("z", CFInt), ("a", CFInt)] Nothing
      desugar row `shouldBe` [Just expected]
      desugar (record row) `shouldBe` [Just (CFRecord expected)]
      desugar (record kindedEmpty) `shouldBe` [Just (CFRecord (CFRow [] Nothing))]

    specify "preserves unkinded closed rows" $ do
      desugar (field "x" int emptyRow) `shouldBe` [Just (CFRow [("x", CFInt)] Nothing)]
      desugar (record emptyRow) `shouldBe` [Just (CFRecord (CFRow [] Nothing))]

    specify "retains named and skolem open tails" $ do
      let expected = CFRow [("x", CFInt)] (Just (CFTypeVar "r"))
          expectedSkolem = CFRow [("x", CFInt)] (Just (CFTypeVar "r$scope0"))
          named = field "x" int (T.TypeVar a "r")
          skolem = field "x" int (T.Skolem a "r" Nothing 0 (T.SkolemScope 0))
      desugar named `shouldBe` [Just expected]
      desugar (record named) `shouldBe` [Just (CFRecord expected)]
      desugar skolem `shouldBe` [Just expectedSkolem]
      desugar (record skolem) `shouldBe` [Just (CFRecord expectedSkolem)]

    specify "keeps polymorphic fields in closed records" $ do
      let ty = T.ForAll a T.TypeVarInvisible "a" Nothing
            (record $ field "x" (T.TypeVar a "a") kindedEmpty) Nothing
      desugar ty `shouldBe`
        [Just (CFForAll ["a"] (CFRecord (CFRow [("x", CFTypeVar "a")] Nothing)))]

    specify "distinguishes nested closed and open records" $ do
      let closed = record $ field "value" int kindedEmpty
          open = record $ field "value" int (T.TypeVar a "r")
          outer = record $ field "closed" closed $ field "open" open kindedEmpty
      desugar outer `shouldBe` [Just (CFRecord (CFRow
        [ ("closed", CFRecord (CFRow [("value", CFInt)] Nothing))
        , ("open", CFRecord (CFRow [("value", CFInt)] (Just (CFTypeVar "r"))))
        ] Nothing))]

    specify "does not classify an unknown tail as closed" $ do
      let row = field "x" int (T.TUnknown a 0)
          expected = CFRow [("x", CFInt)] (Just CFAny)
      desugar row `shouldBe` [Just expected]
      desugar (record row) `shouldBe` [Just (CFRecord expected)]

  context "typed FFI effect contracts" $ do
    let a = NullSourceAnn
        qualified owner name = Qualified (ByModuleName (ModuleName owner)) (ProperName name)
        effectFnAff = qualified "Effect.Aff.Compat" "EffectFnAff"
        otherEffectFnAff = qualified "Application" "EffectFnAff"
        callback = qualified "Effect.Aff.Compat" "EffectFnCb"
        effectFn1 = qualified "Effect.Uncurried" "EffectFn1"
        wrapped = qualified "Application" "Wrapped"
        alias = qualified "Application" "Alias"
        textAlias = qualified "Application" "TextAlias"
        recursiveAlias = qualified "Application" "RecursiveAlias"
        growingNewtype = qualified "Application" "Grow"
        identityNewtype = qualified "Application" "Identity"
        constructor = T.TypeConstructor a
        apply = T.TypeApp a
        variable = T.TypeVar a
        int = constructor C.Int
        kind = constructor C.Type
        unaryKind = kind -:> kind
        unit = constructor (qualified "Data.Unit" "Unit")
        newtypeKind body = DataType Newtype [("a", Nothing, Representational)] [(ProperName "Wrap", [body])]
        environment = initEnvironment
          { types = M.union (M.fromList
              [ (effectFnAff, (unaryKind, newtypeKind (variable "a")))
              , (otherEffectFnAff, (unaryKind, newtypeKind (variable "a")))
              , (wrapped, (unaryKind, newtypeKind (apply (constructor callback) (variable "a"))))
              , (effectFn1, (kind -:> unaryKind, ExternData [Representational, Representational]))
              , (callback, (unaryKind, TypeSynonym))
              , (alias, (unaryKind, TypeSynonym))
              , (textAlias, (kind, TypeSynonym))
              , (recursiveAlias, (kind, TypeSynonym))
              , (growingNewtype, (unaryKind, newtypeKind $
                  apply (constructor growingNewtype) (apply (constructor C.Array) (variable "a"))))
              , (identityNewtype, (unaryKind, newtypeKind (variable "a")))
              ]) (types initEnvironment)
          , typeSynonyms = M.fromList
              [ (callback, ([("a", Nothing)], apply (apply (constructor effectFn1) (variable "a")) unit))
              , (alias, ([("a", Nothing)], apply (constructor effectFnAff) (variable "a")))
              , (textAlias, ([], constructor C.String))
              , (recursiveAlias, ([], constructor recursiveAlias))
              ]
          }
        desugar ty =
          [ annotationType
          | ((_, _, annotationType, _, _), _) <- moduleForeign $ moduleToCoreFn environment $
              A.Module ss [] mn [A.ExternDeclaration (ss, []) (Ident "value") ty] (Just [])
          ]

    specify "preserves the qualified EffectFnAff contract with its instantiated result" $ do
      desugar (apply (constructor effectFnAff) int)
        `shouldBe` [Just (CFAdt effectFnAff [CFInt])]
      desugar (apply (constructor effectFnAff) (apply (constructor C.Array) int))
        `shouldBe` [Just (CFAdt effectFnAff [CFArray CFInt])]
      desugar (apply (constructor otherEffectFnAff) int) `shouldBe` [Just CFInt]

    specify "resolves aliases without erasing EffectFnAff identity" $ do
      desugar (apply (constructor alias) int)
        `shouldBe` [Just (CFAdt effectFnAff [CFInt])]
      desugar (constructor textAlias) `shouldBe` [Just CFString]

    specify "substitutes aliases nested inside a newtype representation" $ do
      desugar (apply (constructor wrapped) int)
        `shouldBe` [Just (CFAdt effectFn1 [CFInt, CFUnit])]
      desugar (apply (constructor callback) (apply (constructor C.Array) int))
        `shouldBe` [Just (CFAdt effectFn1 [CFArray CFInt, CFUnit])]

    specify "terminates recursive synonym expansion" $ do
      desugar (constructor recursiveAlias) `shouldBe` [Just (CFAdt recursiveAlias [])]

    specify "terminates a recursive newtype whose argument grows during expansion" $ do
      desugar (apply (constructor growingNewtype) int)
        `shouldBe` [Just (CFAdt growingNewtype [CFArray CFInt])]

    specify "fully unwraps a nested newtype when each expansion makes progress" $ do
      desugar (apply (constructor identityNewtype) (apply (constructor identityNewtype) int))
        `shouldBe` [Just CFInt]

  context "scoped type identities" $ do
    let a = NullSourceAnn
        variable = T.TypeVar a
        skolem name scope = T.Skolem a name Nothing scope (T.SkolemScope scope)
        forallAt name scope body = T.ForAll a T.TypeVarInvisible name Nothing body scope
        function x y = T.TypeApp a (T.TypeApp a (T.TypeConstructor a C.Function) x) y
        desugar ty =
          [ annotationType
          | ((_, _, annotationType, _, _), _) <- moduleForeign $ moduleToCoreFn initEnvironment $
              A.Module ss [] mn [A.ExternDeclaration (ss, []) (Ident "value") ty] (Just [])
          ]

    specify "links a scoped forall to skolems in separate annotations" $ do
      let scope = Just (T.SkolemScope 1)
          scoped = CFTypeVar "a$scope1"
      desugar (forallAt "a" scope $ function (variable "a") (variable "a"))
        `shouldBe` [Just (CFForAll ["a$scope1"] (CFFunc [scoped] scoped))]
      desugar (function (skolem "a" 1) (skolem "a" 1))
        `shouldBe` [Just (CFFunc [scoped] scoped)]

    specify "distinguishes an existential from an outer variable with the same name" $ do
      let outer = CFTypeVar "a$scope1"
          inner = CFTypeVar "a$scope2"
          callback = forallAt "a" (Just (T.SkolemScope 2)) $
            function (variable "a") (skolem "a" 1)
          ty = forallAt "a" (Just (T.SkolemScope 1)) $
            function callback (variable "a")
      desugar ty `shouldBe`
        [Just (CFForAll ["a$scope1"] (CFFunc [CFForAll ["a$scope2"] (CFFunc [inner] outer)] outer))]
      desugar (function (skolem "a" 2) (skolem "a" 1))
        `shouldBe` [Just (CFFunc [inner] outer)]

    specify "keeps unscoped quantifiers lexical and does not capture free skolems" $ do
      let ty = forallAt "a" Nothing $ function (variable "a") (skolem "a" 1)
      desugar ty `shouldBe`
        [Just (CFForAll ["a"] (CFFunc [CFTypeVar "a"] (CFTypeVar "a$scope1")))]

  specify "should parse version" $ do
    let v = Version [0, 13, 6] []
        m = Module ss [] mn mp [] [] M.empty [] [] [] []
        r = fst <$> parseModule (moduleToJSON v m)
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Success v' -> v' `shouldBe` v

  specify "should parse an empty module" $ do
    let r = parseMod $ Module ss [] mn mp [] [] M.empty [] []
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Success m -> moduleName m `shouldBe` mn

  specify "should parse source span" $ do
    let r = parseMod $ Module ss [] mn mp [] [] M.empty [] []
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Success m -> moduleSourceSpan m `shouldBe` ss

  specify "should parse module path" $ do
    let r = parseMod $ Module ss [] mn mp [] [] M.empty [] []
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Success m -> modulePath m `shouldBe` mp

  specify "should parse imports" $ do
    let r = parseMod $ Module ss [] mn mp [(ann, mn)] [] M.empty [] []
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Success m -> moduleImports m `shouldBe` [(ann, mn)]

  specify "should parse exports" $ do
    let r = parseMod $ Module ss [] mn mp [] [Ident "exp"] M.empty [] []
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Success m -> moduleExports m `shouldBe` [Ident "exp"]

  specify "should parse re-exports" $ do
    let r = parseMod $ Module ss [] mn mp [] [] (M.singleton (ModuleName "Example.A") [Ident "exp"]) [] []
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Success m -> moduleReExports m `shouldBe` M.singleton (ModuleName "Example.A") [Ident "exp"]


  specify "should parse foreign" $ do
    let r = parseMod $ Module ss [] mn mp [] [] M.empty [(ann, Ident "exp")] []
    r `shouldSatisfy` isSuccess
    case r of
      Error _   -> return ()
      Success m -> moduleForeign m `shouldBe` [(ann, Ident "exp")]

  context "Expr" $ do
    specify "should parse literals" $ do
      let m = Module ss [] mn mp [] [] M.empty []
                [ NonRec ann (Ident "x1") $ Literal ann (NumericLiteral (Left 1))
                , NonRec ann (Ident "x2") $ Literal ann (NumericLiteral (Right 1.0))
                , NonRec ann (Ident "x3") $ Literal ann (StringLiteral (mkString "abc"))
                , NonRec ann (Ident "x4") $ Literal ann (CharLiteral 'c')
                , NonRec ann (Ident "x5") $ Literal ann (BooleanLiteral True)
                , NonRec ann (Ident "x6") $ Literal ann (ArrayLiteral [Literal ann (CharLiteral 'a')])
                , NonRec ann (Ident "x7") $ Literal ann (ObjectLiteral [(mkString "a", Literal ann (CharLiteral 'a'))])
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse Constructor" $ do
      let m = Module ss [] mn mp [] [] M.empty []
                [ NonRec ann (Ident "constructor") $ Constructor ann (ProperName "Either") (ProperName "Left") [Ident "value0"] ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse Accessor" $ do
      let m = Module ss [] mn mp [] [] M.empty []
                [ NonRec ann (Ident "x") $
                    Accessor ann (mkString "field") (Literal ann $ ObjectLiteral [(mkString "field", Literal ann (NumericLiteral (Left 1)))]) ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse ObjectUpdate" $ do
      let m = Module ss [] mn mp [] [] M.empty []
                [ NonRec ann (Ident "objectUpdate") $
                    ObjectUpdate ann
                      (Literal ann $ ObjectLiteral [(mkString "field", Literal ann (StringLiteral (mkString "abc")))])
                      (Just [mkString "unchangedField"])
                      [(mkString "field", Literal ann (StringLiteral (mkString "xyz")))]
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse Abs" $ do
      let m = Module ss [] mn mp [] [] M.empty []
                [ NonRec ann (Ident "abs")
                    $ Abs ann (Ident "x") (Var ann (Qualified (ByModuleName mn) (Ident "x")))
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse App" $ do
      let m = Module ss [] mn mp [] [] M.empty []
                [ NonRec ann (Ident "app")
                    $ App ann
                        (Abs ann (Ident "x") (Var ann (Qualified ByNullSourcePos (Ident "x"))))
                        (Literal ann (CharLiteral 'c'))
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse UnusedIdent in Abs" $ do
      let i = NonRec ann (Ident "f") (Abs ann UnusedIdent (Var ann (Qualified ByNullSourcePos (Ident "x"))))
      let r = parseMod $ Module ss [] mn mp [] [] M.empty [] [i]
      r `shouldSatisfy` isSuccess
      case r of
        Error _ -> pure ()
        Success Module{..} ->
          moduleDecls `shouldBe` [i]

    specify "should parse Case" $ do
      let m = Module ss [] mn mp [] [] M.empty []
                [ NonRec ann (Ident "case") $
                    Case ann [Var ann (Qualified ByNullSourcePos (Ident "x"))]
                      [ CaseAlternative
                        [ NullBinder ann ]
                        (Right (Literal ann (CharLiteral 'a')))
                      ]
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse Case with guards" $ do
      let m = Module ss [] mn mp [] [] M.empty []
                [ NonRec ann (Ident "case") $
                    Case ann [Var ann (Qualified ByNullSourcePos (Ident "x"))]
                      [ CaseAlternative
                        [ NullBinder ann ]
                        (Left [(Literal ann (BooleanLiteral True), Literal ann (CharLiteral 'a'))])
                      ]
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse Let" $ do
      let m = Module ss [] mn mp [] [] M.empty []
                [ NonRec ann (Ident "case") $
                    Let ann
                      [ Rec [((ann, Ident "a"), Var ann (Qualified ByNullSourcePos (Ident "x")))] ]
                      (Literal ann (BooleanLiteral True))
                ]
      parseMod m `shouldSatisfy` isSuccess

  context "Meta" $ do
    specify "should parse IsConstructor" $ do
      let m = Module ss [] mn mp [] [] M.empty []
                [ NonRec (ss, [], Nothing, Just (IsConstructor ProductType [Ident "x"]), Nothing) (Ident "x") $
                  Literal (ss, [], Nothing, Just (IsConstructor SumType []), Nothing) (CharLiteral 'a')
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse IsNewtype" $ do
      let m = Module ss [] mn mp [] [] M.empty []
                [ NonRec (ss, [], Nothing, Just IsNewtype, Nothing) (Ident "x") $
                  Literal ann (CharLiteral 'a')
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse IsTypeClassConstructor" $ do
      let m = Module ss [] mn mp [] [] M.empty []
                [ NonRec (ss, [], Nothing, Just IsTypeClassConstructor, Nothing) (Ident "x") $
                  Literal ann (CharLiteral 'a')
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse IsForeign" $ do
      let m = Module ss [] mn mp [] [] M.empty []
                [ NonRec (ss, [], Nothing, Just IsForeign, Nothing) (Ident "x") $
                  Literal ann (CharLiteral 'a')
                ]
      parseMod m `shouldSatisfy` isSuccess

  context "Binders" $ do
    specify "should parse LiteralBinder" $ do
      let m = Module ss [] mn mp [] [] M.empty []
                [ NonRec ann (Ident "case") $
                    Case ann [Var ann (Qualified ByNullSourcePos (Ident "x"))]
                      [ CaseAlternative
                        [ LiteralBinder ann (BooleanLiteral True) ]
                        (Right (Literal ann (CharLiteral 'a')))
                      ]
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse VarBinder" $ do
      let m = Module ss [] mn mp [] [] M.empty []
                [ NonRec ann (Ident "case") $
                    Case ann [Var ann (Qualified ByNullSourcePos (Ident "x"))]
                      [ CaseAlternative
                        [ ConstructorBinder
                            ann
                            (Qualified (ByModuleName (ModuleName "Data.Either")) (ProperName "Either"))
                            (Qualified ByNullSourcePos (ProperName "Left"))
                            [VarBinder ann (Ident "z")]
                        ]
                        (Right (Literal ann (CharLiteral 'a')))
                      ]
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse NamedBinder" $ do
      let m = Module ss [] mn mp [] [] M.empty []
                [ NonRec ann (Ident "case") $
                    Case ann [Var ann (Qualified ByNullSourcePos (Ident "x"))]
                      [ CaseAlternative
                        [ NamedBinder ann (Ident "w") (NamedBinder ann (Ident "w'") (VarBinder ann (Ident "w''"))) ]
                        (Right (Literal ann (CharLiteral 'a')))
                      ]
                ]
      parseMod m `shouldSatisfy` isSuccess

  context "Comments" $ do
    specify "should parse LineComment" $ do
      let m = Module ss [ LineComment "line" ] mn mp [] [] M.empty [] []
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse BlockComment" $ do
      let m = Module ss [ BlockComment "block" ] mn mp [] [] M.empty [] []
      parseMod m `shouldSatisfy` isSuccess

  context "CoreFnType" $ do
    specify "should parse CFInt" $ do
      let annWithType = (ss, [], Just CFInt, Nothing, Nothing)
          m = Module ss [] mn mp [] [] M.empty []
                [ NonRec annWithType (Ident "x") $
                  Literal annWithType (NumericLiteral (Left 1))
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse CFRecord and CFRow" $ do
      let recordTy = CFRecord (CFRow [(mkString "foo", CFString)] Nothing)
          annWithType = (ss, [], Just recordTy, Nothing, Nothing)
          m = Module ss [] mn mp [] [] M.empty []
                [ NonRec annWithType (Ident "x") $
                  Literal annWithType (NumericLiteral (Left 1))
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse CFFunc" $ do
      let funcTy = CFFunc [CFInt, CFString] CFBoolean
          annWithType = (ss, [], Just funcTy, Nothing, Nothing)
          m = Module ss [] mn mp [] [] M.empty []
                [ NonRec annWithType (Ident "x") $
                  Literal annWithType (NumericLiteral (Left 1))
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse CFAdt" $ do
      let adtTy = CFAdt (Qualified ByNullSourcePos (ProperName "Maybe")) [CFInt]
          annWithType = (ss, [], Just adtTy, Nothing, Nothing)
          m = Module ss [] mn mp [] [] M.empty []
                [ NonRec annWithType (Ident "x") $
                  Literal annWithType (NumericLiteral (Left 1))
                ]
      parseMod m `shouldSatisfy` isSuccess
