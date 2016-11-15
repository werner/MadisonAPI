module Formula.Evaluator where

import Control.Exception

import Formula.Parser
import Formula.Types
import Formula.Functions

import qualified Data.Map as M

printStmt :: Stmt -> SymTab ->  String
printStmt statement vars = 
    case (evaluateStmt statement vars) of
      ResultDouble double      -> show(double)
      ResultBool bool          -> show(bool)
      ResultSymTab (_, symTab) -> show(symTab)

evaluateStmt :: Stmt -> SymTab -> ResultStmt
evaluateStmt statement vars =
    case statement of
      StmtAExpr expr -> ResultDouble $ evaluateExpr expr vars
      StmtBExpr expr -> ResultBool $ evaluateBExpr expr
      Function string args -> evaluateFunction $ Function string args
      If bexpr statement1 statement2 -> if evaluateBExpr bexpr then 
                                          evaluateStmt statement1 vars
                                        else
                                          evaluateStmt statement2 vars
      _ -> throw(StatementNotImplemented $ show statement)


evaluateExpr :: AExpr -> SymTab -> Double
evaluateExpr expression variables = 
    case expression of
      Var val                      -> fst $ lookUp val variables
      NumConst expr                -> numConst (NumConst expr)
      ABinary Add      expr1 expr2 -> evaluateExpr expr1 variables +  evaluateExpr expr2 variables
      ABinary Subtract expr1 expr2 -> evaluateExpr expr1 variables -  evaluateExpr expr2 variables
      ABinary Multiply expr1 expr2 -> evaluateExpr expr1 variables *  evaluateExpr expr2 variables
      ABinary Divide   expr1 expr2 -> evaluateExpr expr1 variables /  evaluateExpr expr2 variables
      ABinary Power    expr1 expr2 -> evaluateExpr expr1 variables ** evaluateExpr expr2 variables
      _ -> throw(ExpressionNotImplemented $ show expression)

evaluateBExpr :: BExpr -> Bool
evaluateBExpr expression = 
    case expression of
      BoolConst expr                -> boolConst (BoolConst expr)
      Not expr                      -> boolConst (Not expr)
      BBinary And     bexpr1 bexpr2 -> evaluateBExpr bexpr1 && evaluateBExpr bexpr2
      BBinary Or      bexpr1 bexpr2 -> evaluateBExpr bexpr1 || evaluateBExpr bexpr2
      RBinary Greater bexpr1 bexpr2 -> evaluateExpr bexpr1  M.empty >  evaluateExpr bexpr2 M.empty
      RBinary Less    bexpr1 bexpr2 -> evaluateExpr bexpr1  M.empty <  evaluateExpr bexpr2 M.empty
      RBinary Equal    bexpr1 bexpr2 -> evaluateExpr bexpr1 M.empty == evaluateExpr bexpr2 M.empty
      RBinary NotEqual bexpr1 bexpr2 -> evaluateExpr bexpr1 M.empty /= evaluateExpr bexpr2 M.empty
      RBinary GreaterOrEqual bexpr1 bexpr2 -> evaluateExpr bexpr1 M.empty >= evaluateExpr bexpr2 M.empty
      RBinary LessOrEqual    bexpr1 bexpr2 -> evaluateExpr bexpr1 M.empty <= evaluateExpr bexpr2 M.empty

lookUp :: String -> SymTab -> (Double, SymTab)
lookUp str symTab = 
    case M.lookup str symTab of
      Just v -> (v, symTab)
      Nothing -> throw(VariableNotFound $ str)

addSymbol :: String -> Double -> SymTab -> ((), SymTab)
addSymbol str val symTab = 
    let symTab' = M.insert str val symTab
    in ((), symTab')
