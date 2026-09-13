{-# LANGUAGE DoAndIfThenElse #-}

module TestCoreFn (spec) where

import Prelude

import Data.Aeson (Result(..), Value)
import Data.Aeson.Types (parse)
import Data.Map as M
import Data.Version (Version(..))

import Language.PureScript.AST qualified as A
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (pattern NullSourceAnn, SourcePos(..), SourceSpan(..))
import Language.PureScript.Comments (Comment(..))
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.CoreFn (Ann, Bind(..), Binder(..), CaseAlternative(..), ConstructorType(..), Expr(..), Meta(..), Module(..), ssAnn, CoreFnType(..))
import Language.PureScript.CoreFn.Desugar (moduleToCoreFn)
import Language.PureScript.CoreFn.Module (DataDecl, ClassDecl)
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Language.PureScript.CoreFn.ToJSON (moduleToJSON)
import Language.PureScript.Environment (initEnvironment)
import Language.PureScript.Label (Label(..))
import Language.PureScript.Names (pattern ByNullSourcePos, Ident(..), ModuleName(..), ProperName(..), Qualified(..), QualifiedBy(..))
import Language.PureScript.PSString (mkString)
import Language.PureScript.Types qualified as T

import Test.Hspec (Spec, context, shouldBe, shouldSatisfy, specify)

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

spec :: Spec
spec = context "CoreFnFromJson" $ do
  let mn = ModuleName "Example.Main"
      mp = "src/Example/Main.purs"
      ss = SourceSpan mp (SourcePos 0 0) (SourcePos 0 0)
      ann = ssAnn ss

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
          | ((_, _, annotationType, _), _) <- moduleForeign $ moduleToCoreFn initEnvironment $
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

  context "scoped type identities" $ do
    let a = NullSourceAnn
        variable = T.TypeVar a
        skolem name scope = T.Skolem a name Nothing scope (T.SkolemScope scope)
        forallAt name scope body = T.ForAll a T.TypeVarInvisible name Nothing body scope
        function x y = T.TypeApp a (T.TypeApp a (T.TypeConstructor a C.Function) x) y
        desugar ty =
          [ annotationType
          | ((_, _, annotationType, _), _) <- moduleForeign $ moduleToCoreFn initEnvironment $
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
                [ NonRec (ss, [], Nothing, Just (IsConstructor ProductType [Ident "x"])) (Ident "x") $
                  Literal (ss, [], Nothing, Just (IsConstructor SumType [])) (CharLiteral 'a')
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse IsNewtype" $ do
      let m = Module ss [] mn mp [] [] M.empty []
                [ NonRec (ss, [], Nothing, Just IsNewtype) (Ident "x") $
                  Literal ann (CharLiteral 'a')
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse IsTypeClassConstructor" $ do
      let m = Module ss [] mn mp [] [] M.empty []
                [ NonRec (ss, [], Nothing, Just IsTypeClassConstructor) (Ident "x") $
                  Literal ann (CharLiteral 'a')
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse IsForeign" $ do
      let m = Module ss [] mn mp [] [] M.empty []
                [ NonRec (ss, [], Nothing, Just IsForeign) (Ident "x") $
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
      let annWithType = (ss, [], Just CFInt, Nothing)
          m = Module ss [] mn mp [] [] M.empty []
                [ NonRec annWithType (Ident "x") $
                  Literal annWithType (NumericLiteral (Left 1))
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse CFRecord and CFRow" $ do
      let recordTy = CFRecord (CFRow [(mkString "foo", CFString)] Nothing)
          annWithType = (ss, [], Just recordTy, Nothing)
          m = Module ss [] mn mp [] [] M.empty []
                [ NonRec annWithType (Ident "x") $
                  Literal annWithType (NumericLiteral (Left 1))
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse CFFunc" $ do
      let funcTy = CFFunc [CFInt, CFString] CFBoolean
          annWithType = (ss, [], Just funcTy, Nothing)
          m = Module ss [] mn mp [] [] M.empty []
                [ NonRec annWithType (Ident "x") $
                  Literal annWithType (NumericLiteral (Left 1))
                ]
      parseMod m `shouldSatisfy` isSuccess

    specify "should parse CFAdt" $ do
      let adtTy = CFAdt (Qualified ByNullSourcePos (ProperName "Maybe")) [CFInt]
          annWithType = (ss, [], Just adtTy, Nothing)
          m = Module ss [] mn mp [] [] M.empty []
                [ NonRec annWithType (Ident "x") $
                  Literal annWithType (NumericLiteral (Left 1))
                ]
      parseMod m `shouldSatisfy` isSuccess
