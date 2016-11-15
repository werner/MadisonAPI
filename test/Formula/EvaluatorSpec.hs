module Formula.EvaluatorSpec (main, spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Data.Aeson (Value(..), object, (.=))

import qualified Data.Map as M

import Formula.Parser
import Formula.Types
import Formula.Functions
import Formula.Evaluator

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "working formulas" $ do
    it "performs simple arithmetic equations" $ do
      printStmt (parseString "43+23.56") M.empty `shouldBe` "66.56"
      printStmt (parseString "50.43-10") M.empty `shouldBe` "40.43"
      printStmt (parseString "78.2*6")   M.empty `shouldBe` "469.20000000000005"
      printStmt (parseString "7.9/4")    M.empty `shouldBe` "1.975"
      printStmt (parseString "6.3^4.2")  M.empty `shouldBe` "2276.304241925657"
      printStmt (parseString "87.23+45.34-(8+9)*6.3^4.2") M.empty `shouldBe` "-38564.602112736175"

    it "works with conditional" $ do
      printStmt (parseString "if 5 > 2 then 9 else 8.2")      M.empty `shouldBe` "9.0"
      printStmt (parseString "if 13.21 < 20 then 34 else 67") M.empty `shouldBe` "34.0"
      printStmt (parseString "if 8.9 < 20 then 34 else 67")   M.empty `shouldBe` "34.0"
      printStmt (parseString "if ((8.9 < 20) and (30 > 23)) then 879 else 67") M.empty `shouldBe` "879.0"
      printStmt (parseString "if ((8.9 < 20) or (30 < 23)) then 879 else 67")  M.empty `shouldBe` "879.0"
      printStmt (parseString "if ((8.9 > 20) or (30 < 23)) then 879 else 67")  M.empty `shouldBe` "67.0"
      printStmt (parseString "if (56.34 == 56.34) then 1023 else 890")  M.empty `shouldBe` "1023.0"
      printStmt (parseString "if (not true) then 879 else 67") M.empty `shouldBe` "67.0"

    it "performs some functions" $ do
      printStmt (parseString "max(1,4,2,6,3)") M.empty `shouldBe` "6.0"
      printStmt (parseString "min(1,4,2,6,3)") M.empty `shouldBe` "1.0"
      printStmt (parseString "avg(5,4.8,2,6,3)") M.empty `shouldBe` "4.16"
      printStmt (parseString "product(9,1.8,7,6,3)") M.empty `shouldBe` "2041.1999999999998"

  describe "exceptions" $ do
    it "throw an error on wrong syntax" $ do
      evaluate (parseString "a:=1º3") `shouldThrow` anyException
