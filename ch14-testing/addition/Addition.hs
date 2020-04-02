module Addition where
import Test.Hspec
import ProblemEulerOne

sayHello :: IO ()
sayHello = putStrLn "Hello!"

main :: IO ()
main = hspec $ do
  describe "Test ProblemEulerOne" $ do
    it "\"isDividedBy 5 455\" should be True" $ do
      (isDividedBy 5 455) `shouldBe` True
    it "\"isDividedBy 3 455\" should be False" $ do
      (isDividedBy 3 455) `shouldBe` False
    it "\"multiplesUpto\" test 1" $ do
      (null (multiplesUpto 0)) `shouldBe` True
    it "\"multiplesUpto\" test 2" $ do
      (multiplesUpto 10) `shouldBe` [3,5,6,9,10] 
    it "\"multipleUpto\" test 3" $ do
      (length (multiplesUpto 1000)) `shouldBe` 467
    it "\"sumOfMultiples\" test 1" $ do
      (sumOfMultiples 0) `shouldBe` 0
    it "\"sumOfMultiples\" test 2" $ do
      (sumOfMultiples 10) `shouldBe` 33
    it "\"sumOfMultiples\" test 3" $ do
      (sumOfMultiples 1000) `shouldBe` 234168
    

    