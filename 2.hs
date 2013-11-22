import Test.Hspec

fiboSummify limit = otherFiboSum 0 1 0 limit

otherFiboSum lower upper sum limit
  | upper > limit = sum
  | otherwise     = otherFiboSum upper (lower + upper) (addEvenFib upper sum) limit

addEvenFib upper sum =
  if upper `mod` 2 == 0
    then sum + upper
    else sum

main = hspec $ do
  describe "evenFiboSummify" $ do
    it "sums the even fibonacci numbers from 1 to 2" $ do
      fiboSummify 2 `shouldBe` 2
    it "sums the even fibonacci numbers from 1 to 3" $ do
      fiboSummify 3 `shouldBe` 2
    it "sums the even fibonacci numbers from 1 to 5" $ do
      fiboSummify 5 `shouldBe` 2
    it "sums the even fibonacci numbers from 1 to 8" $ do
      fiboSummify 8 `shouldBe` 10
    it "sums the even fibonacci numbers from 1 to 13" $ do
      fiboSummify 34 `shouldBe` 44
    it "tells me what the sum is for even fibs up to 4 million" $ do
      fiboSummify 3999999 `shouldBe` 4613732
