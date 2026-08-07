module Main where
import Data.Maybe
import Language.PureScript.CoreFn.Ann
import Language.PureScript.Names
import Data.Text (pack)

main :: IO ()
main = do
  let t1 = Just (CFFunc [CFString] CFString)
  let t2 = Just (CFFunc [CFAdt (Qualified (ByModuleName (ModuleName "Core.Mod.Image.Image")) (ProperName "Image")) []] (CFAdt (Qualified (ByModuleName (ModuleName "Core.Mod.Image.Image")) (ProperName "Image")) []))
  print (t1 == t2)
  let t3 = Just (CFFunc [CFAny] CFAny)
  print (t1 == t3)
