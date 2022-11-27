import Token
import Tokenizer
import Converter
import Test.Hspec


edgeCases :: String -> Spec
edgeCases label = describe label $ do
  it "Empty expression" $ do
    toRpn [] `shouldBe` Right []


testAdd :: String -> Spec
testAdd label = describe label $ do
  it "1 + 1" $ do
    let infx = [Literal 1, Add, Literal 1]
    let rpn = [Literal 1, Literal 1, Add]
    toRpn infx `shouldBe` Right rpn

  it "20 + 15 + 600" $ do
    let infx = [Literal 20, Add, Literal 15, Add, Literal 600]
    let rpn = [Literal 20, Literal 15, Add, Literal 600, Add]
    toRpn infx `shouldBe` Right rpn

  it "1 + 2 + 3 + 4" $ do
    let infx = [Literal 1, Add, Literal 2, Add, Literal 3, Add, Literal 4]
    let rpn = [Literal 1, Literal 2, Add, Literal 3, Add, Literal 4, Add]
    toRpn infx `shouldBe` Right rpn


testSub :: String -> Spec
testSub label = describe label $ do
  it "1 - 1" $ do
    let infx = [Literal 1, Sub, Literal 1]
    let rpn = [Literal 1, Literal 1, Sub]
    toRpn infx `shouldBe` Right rpn

  it "0 - 15 - 29" $ do
    let infx = [Literal 0, Sub, Literal 15, Sub, Literal 29]
    let rpn = [Literal 0, Literal 15, Sub, Literal 29, Sub]
    toRpn infx `shouldBe` Right rpn

  it "4 - 3 - 2 - 1" $ do
    let infx = [Literal 4, Sub, Literal 3, Sub, Literal 2, Sub, Literal 1]
    let rpn = [Literal 4, Literal 3, Sub, Literal 2, Sub, Literal 1, Sub]
    toRpn infx `shouldBe` Right rpn


testMul :: String -> Spec
testMul label = describe label $ do
  it "1 * 1" $ do
    let infx = [Literal 1, Mul, Literal 1]
    let rpn = [Literal 1, Literal 1, Mul]
    toRpn infx `shouldBe` Right rpn

  it "1 * 15 * 201" $ do
    let infx = [Literal 1, Mul, Literal 15, Mul, Literal 201]
    let rpn = [Literal 1, Literal 15, Mul, Literal 201, Mul]
    toRpn infx `shouldBe` Right rpn

  it "4 * 3 * 2 * 1" $ do
    let infx = [Literal 4, Mul, Literal 3, Mul, Literal 2, Mul, Literal 1]
    let rpn = [Literal 4, Literal 3, Mul, Literal 2, Mul, Literal 1, Mul]
    toRpn infx `shouldBe` Right rpn


testDiv :: String -> Spec
testDiv label = describe label $ do
  it "1 / 1" $ do
    let infx = [Literal 1, Div, Literal 1]
    let rpn = [Literal 1, Literal 1, Div]
    toRpn infx `shouldBe` Right rpn

  it "15 / 3 / 5" $ do
    let infx = [Literal 15, Div, Literal 3, Div, Literal 5]
    let rpn = [Literal 15, Literal 3, Div, Literal 5, Div]
    toRpn infx `shouldBe` Right rpn

  it "4 / 3 / 2 / 1" $ do
    let infx = [Literal 4, Div, Literal 3, Div, Literal 2, Div, Literal 1]
    let rpn = [Literal 4, Literal 3, Div, Literal 2, Div, Literal 1, Div]
    toRpn infx `shouldBe` Right rpn


testMixed :: String -> Spec
testMixed label = describe label $ do
  it "1 * 2 + 3" $ do
    let infx = [Literal 1, Mul, Literal 2, Add, Literal 3]
    let rpn = [Literal 1, Literal 2, Mul, Literal 3, Add]
    toRpn infx `shouldBe` Right rpn

  it "1 + 2 * 3" $ do
    let infx = [Literal 1, Add, Literal 2, Mul, Literal 3]
    let rpn = [Literal 1, Literal 2, Literal 3, Mul, Add]
    toRpn infx `shouldBe` Right rpn

  it "5 * 2 / 3 + 15 - 2" $ do
    let infx = [Literal 5, Mul, Literal 2, Div, Literal 3, Add, Literal 15, Sub, Literal 2]
    let rpn = [Literal 5, Literal 2, Mul, Literal 3, Div, Literal 15, Add, Literal 2, Sub]
    toRpn infx `shouldBe` Right rpn


testParens :: String -> Spec
testParens label = describe label $ do
  it "(1 + 2) * 3" $ do
    let infx = [OpenParens, Literal 1, Add, Literal 2, CloseParens, Mul, Literal 3]
    let rpn = [Literal 1, Literal 2, Add, Literal 3, Mul]
    toRpn infx `shouldBe` Right rpn

  it "1 + (2 * 3)" $ do
    let infx = [Literal 1, Add, OpenParens, Literal 2, Mul, Literal 3, CloseParens]
    let rpn = [Literal 1, Literal 2, Literal 3, Mul, Add]
    toRpn infx `shouldBe` Right rpn

  it "1 + ((2 * 3) / 4 - 5)" $ do
     let infx = [Literal 1, Add, OpenParens, OpenParens, Literal 2, Mul, Literal 3,
                 CloseParens, Div, Literal 4, Sub, Literal 5, CloseParens]
     let rpn = [Literal 1, Literal 2, Literal 3, Mul, Literal 4, Div, Literal 5, Sub, Add]
     toRpn infx `shouldBe` Right rpn


main :: IO ()
main = hspec $ do
  testAdd "Addition"
  testSub "Subtraction"
  testMul "Multiplication"
  testDiv "Division"
  testMixed "Mixed operators"
  testParens "Nested operators"
  edgeCases "Edge cases"
