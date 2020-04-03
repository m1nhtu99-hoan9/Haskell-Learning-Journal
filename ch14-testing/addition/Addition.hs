module Addition where
import Test.Hspec
import Test.QuickCheck
import ProblemEulerOne

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

-- "cheat" a little so that we can get more Just value
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing) , 
              (3, return (Just a)) ]

  -- frequency :: [(Int, Gen a)] -> Gen a
  
genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c)
 => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

qchkIsDividedByProp1 :: Int -> Int -> Bool
qchkIsDividedByProp1 0 _ = True
qchkIsDividedByProp1 x y = (==) (isDividedBy x y) (isDividedBy x (negate y))

qchkIsDividedByProp2 :: Int -> Int -> Bool
qchkIsDividedByProp2 0 _ = True
qchkIsDividedByProp2 x y = (==) (isDividedBy x y) (isDividedBy x (y + x))

main :: IO ()
main = hspec $ do
  describe "Test Specification of ProblemEulerOne" $ do
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
  describe "Test Properties of ProblemEulerOne" $ do
    it "\"isDividedBy\" evaluates to the same result for (x, y) and (x, -y)" $ do
      property $ qchkIsDividedByProp1 
    it "\"isDividedBy\" evaluates to the same result for (x, y) and (x, y+x)" $ do
      property $ qchkIsDividedByProp2
    --it "\"sumOfMultiples\" function is increasing"
    

    