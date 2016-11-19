{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Formula.Types where

import Control.Exception
import Data.Typeable

import qualified Data.Map as M

data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
            deriving (Show)

data BBinOp = And | Or deriving (Show)

data RBinOp = Greater 
            | Less 
            | GreaterOrEqual 
            | LessOrEqual 
            | Equal 
            | NotEqual  
            deriving (Show)

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
            | Power
              deriving (Show)

data AExpr = Var String
           | NumConst (Either Integer Double)
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
           | ARBinary RBinOp AExpr AExpr
             deriving (Show)

data Stmt = If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
          | StmtAExpr AExpr
          | StmtBExpr BExpr
          | Function String [AExpr]
            deriving (Show)

type SymTab = M.Map String Double

data ResultStmt = ResultBool Bool
                | ResultDouble Double
                | ResultSymTab ((), SymTab)
                deriving (Show)

data StatementException = StatementNotImplemented String
                        | ExpressionNotImplemented String
                        | SyntaxError String
                        | VariableNotFound String
                        | FunctionNotFound String
                        deriving (Show, Typeable)

instance Exception StatementException
