import Test.Hspec

sumOfSquares :: Int -> Int
sumOfSquares max = foldl (\acc num -> acc + (num ^ 2)) 0 [1..max]

sumDifferences max = ((sum [1..max]) ^ 2) - sumOfSquares max

main = hspec $ do
  describe "sumOfSquares" $ do
    it "finds the sum of the squares of the first 1 natural numbers" $ do
      sumOfSquares 1 `shouldBe` 1

    it "finds the sum of the squares of the first 2 natural numbers" $ do
      sumOfSquares 2 `shouldBe` 5

    it "finds the sum of the squares of the first 3 natural numbers" $ do
      sumOfSquares 3 `shouldBe` 14

    it "finds the sum of the squares of the first 100 natural numbers" $ do
      sumOfSquares 100 `shouldBe` 338350

    it "finds the difference between the sumOfSquares and the squareOfSums up to 100" $ do
      sumDifferences 100 `shouldBe` 25164150
