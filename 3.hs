import Test.Hspec

largestPrimeOf number = largestPrimeRecur number 2

largestPrimeRecur number divisor
  | divisor > (number `div` 2)   = number
  | number `mod` divisor == 0 = largestPrimeRecur (number `div` divisor) divisor
  | otherwise                 = largestPrimeRecur number (divisor + 1)

main = hspec $ do
  describe "#largestPrimeOf" $ do
    it "returns the largest prime factor of 3" $ do
      largestPrimeOf 3 `shouldBe` 3

    it "returns the largest prime factor of 4" $ do
      largestPrimeOf 4 `shouldBe` 2

    it "returns the largest prime factor of 6" $ do
      largestPrimeOf 6 `shouldBe` 3

    it "returns the largest prime factor of 14" $ do
      largestPrimeOf 14 `shouldBe` 7

    it "returns the largest prime factor of 3*5*13*19" $ do
      largestPrimeOf (3 * 5 * 13 * 19) `shouldBe` 19

    it "returns the largest prime factor of 600851475143" $ do
      largestPrimeOf (600851475143) `shouldBe` 6857
