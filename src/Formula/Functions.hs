
module Formula.Functions where

import Formula.Types

evaluateFunction :: Stmt -> ResultStmt
evaluateFunction (Function "max" args)     = ResultDouble $ maxFunction args
evaluateFunction (Function "min" args)     = ResultDouble $ minFunction args
evaluateFunction (Function "sum" args)     = ResultDouble $ sumFunction args
evaluateFunction (Function "product" args) = ResultDouble $ productFunction args
evaluateFunction (Function "avg" args)     = ResultDouble $ avgFunction args

maxFunction :: [AExpr] -> Double
maxFunction = foldl (\acc x -> max acc $ numConst x) 0

minFunction :: [AExpr] -> Double
minFunction xs@(x:_) = foldl (\acc y -> min acc $ numConst y) (numConst x) xs

sumFunction :: [AExpr] -> Double
sumFunction = foldl (\acc x -> acc + numConst x) 0

productFunction :: [AExpr] -> Double
productFunction = foldl (\acc x -> acc * numConst x) 1

avgFunction :: [AExpr] -> Double
avgFunction xs = sumFunction xs / fromIntegral(length xs)

numConst :: AExpr -> Double
numConst (NumConst (Left x))  = fromIntegral x
numConst (NumConst (Right x)) = x

boolConst :: BExpr -> Bool
boolConst (BoolConst x) = x
boolConst (Not x) = not $ boolConst x
