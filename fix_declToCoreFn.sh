#!/bin/bash
sed -i '' 's/let ty = getExprType e/let ty = getExprType e <|> ((\(t,_,_) -> t) <$> M.lookup (Qualified (ByModuleName mn) name) (names env))/' src/Language/PureScript/CoreFn/Desugar.hs
