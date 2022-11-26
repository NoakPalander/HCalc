import Token
import Tokenizer
import Test.Hspec


testAdd :: String -> Spec
testAdd label = describe label $ do
  it "1 + 1" $ do
    tokenize "1 + 1" `shouldBe` Right [Literal 1, Add, Literal 1]

  it "2 + 20" $ do
    tokenize "2 + 20" `shouldBe` Right [Literal 2, Add, Literal 20]

  it "512 + 9201 + 19" $ do
    tokenize "512 + 9201 + 19" `shouldBe` Right [Literal 512, Add, Literal 9201, Add, Literal 19]


testSub :: String -> Spec
testSub label = describe label $ do
  it "1 - 1" $ do
    tokenize "1 - 1" `shouldBe` Right [Literal 1, Sub, Literal 1]

  it "2 - 20" $ do
    tokenize "2 - 20" `shouldBe` Right [Literal 2, Sub, Literal 20]

  it "12 - 1920 - 59" $ do
    tokenize "12 - 1920 - 59" `shouldBe` Right [Literal 12, Sub, Literal 1920, Sub, Literal 59]


testMul :: String -> Spec
testMul label = describe label $ do
  it "1 * 1" $ do
    tokenize "1 * 1" `shouldBe` Right [Literal 1, Mul, Literal 1]

  it "2 * 20" $ do
    tokenize "2 * 20" `shouldBe` Right [Literal 2, Mul, Literal 20]

  it "69 * 4 * 82" $ do
    tokenize "69 * 4 * 82" `shouldBe` Right [Literal 69, Mul, Literal 4, Mul, Literal 82]


testDiv :: String -> Spec
testDiv label = describe label $ do
  it "1 / 1" $ do
    tokenize "1 / 1" `shouldBe` Right [Literal 1, Div, Literal 1]

  it "2 / 20" $ do
    tokenize "2 / 20" `shouldBe` Right [Literal 2, Div, Literal 20]

  it "500 / 1 / 16" $ do
    tokenize "500 / 1 / 16" `shouldBe` Right [Literal 500, Div, Literal 1, Div, Literal 16]


testParens :: String -> Spec
testParens label = describe label $ do
  it "(5 * 2) + 15 / (2 - 1)" $ do
    tokenize "(5 * 2) + 15 / (2 - 1)" `shouldBe` Right [
        OpenParens, Literal 5, Mul, Literal 2, CloseParens,
        Add, Literal 15, Div, OpenParens, Literal 2, Sub, Literal 1, CloseParens]


main :: IO ()
main = hspec $ do
  testAdd "Addition"
  testSub "Subtraction"
  testMul "Multiplication"
  testDiv "Division"
  testParens "Parentheses"
