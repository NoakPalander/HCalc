import Token
import Parser
import Test.Hspec
import Data.Either (isLeft)


edgeCases :: String -> Spec
edgeCases label = describe label $ do
  it "1 / 0" $ do
    parse [Literal 1, Literal 0, Div] `shouldSatisfy` isLeft


testAdd :: String -> Spec
testAdd label = describe label $ do
  it "1 + 1" $ do
    parse [Literal 1, Literal 1, Add] `shouldBe` Right 2.0

  it "2 + 20" $ do
    parse [Literal 2, Literal 20, Add] `shouldBe` Right 22.0

  it "512 + 9201 + 19" $ do
    parse [Literal 512, Literal 9201, Add, Literal 19, Add] `shouldBe` Right 9732.0


testSub :: String -> Spec
testSub label = describe label $ do
  it "1 - 1" $ do
    parse [Literal 1, Literal 1, Sub] `shouldBe` Right 0.0

  it "2 - 20" $ do
    parse [Literal 2, Literal 20, Sub] `shouldBe` Right (-18.0)

  it "12 - 1920 - 59" $ do
    parse [Literal 12, Literal 1920, Sub, Literal 59, Sub] `shouldBe` Right (-1967.0)


testMul :: String -> Spec
testMul label = describe label $ do
  it "1 * 1" $ do
    parse [Literal 1, Literal 1, Mul] `shouldBe` Right 1.0

  it "2 * 20" $ do
    parse [Literal 2, Literal 20, Mul] `shouldBe` Right 40.0

  it "69 * 4 * 82" $ do
    parse [Literal 69, Literal 4, Mul, Literal 82, Mul] `shouldBe` Right 22632.0


testDiv :: String -> Spec
testDiv label = describe label $ do
  it "0 / 1" $ do
    parse [Literal 0, Literal 1, Div] `shouldBe` Right 0.0

  it "1 / 1" $ do
    parse [Literal 1, Literal 1, Div] `shouldBe` Right 1.0

  it "2 / 20" $ do
    parse [Literal 2, Literal 20, Div] `shouldBe` Right 0.1

  it "500 / 1 / 16" $ do
    parse [Literal 500, Literal 1, Div, Literal 16, Div] `shouldBe` Right 31.25


testParens :: String -> Spec
testParens label = describe label $ do
  it "10 + (11 * 12)" $ do
    parse [Literal 10, Literal 11, Literal 12, Mul, Add] `shouldBe` Right 142.0

  it "(10 + 11) * 12" $ do
    parse [Literal 10, Literal 11, Add, Literal 12, Mul] `shouldBe` Right 252.0

  it "(5 * 2) + 15 / (2 - 1)" $ do
    parse [Literal 5, Literal 2, Mul, Literal 15, Literal 2, Literal 1, Sub, Div, Add] `shouldBe` Right 25.0

  it "1 + (-15)" $ do
    parse [Literal 1, Literal 15, Sub, Add] `shouldBe` Right (-14.0)

  it "5 - (-5)" $ do
    parse [Literal 5, Literal 5, Sub, Sub] `shouldBe` Right 10.0

main :: IO ()
main = hspec $ do
  testAdd "Addition"
  testSub "Subtraction"
  testMul "Multiplication"
  testDiv "Division"
  testParens "Parentheses"
  edgeCases "Edge cases"
