import Test.Hspec

smallestMultiple maxMultiple = smallestMultipleRecur maxMultiple (maxMultiple * (maxMultiple - 1))

smallestMultipleRecur maxMultiple current
  | evenlyDivisibleByAll maxMultiple current = current
  | otherwise                                = smallestMultipleRecur maxMultiple (current + maxMultiple)

evenlyDivisibleByAll maxMultiple current = not $ any (\n -> current `mod` n /= 0) [1..maxMultiple]

main = hspec $ do
  describe "smallestMultiple" $ do
    it "is 6 for 3" $ do
      smallestMultiple 3 `shouldBe` 6

    it "is 12 for 4" $ do
      smallestMultiple 4 `shouldBe` 12

    it "is 60 for 5" $ do
      smallestMultiple 5 `shouldBe` 60

    it "is 2520 for 10" $ do
      smallestMultiple 10 `shouldBe` 2520

    -- this is super effing slow
    it "is something for 20" $ do
      smallestMultiple 20 `shouldBe` 232792560
