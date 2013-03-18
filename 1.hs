import Test.Hspec

sumMultiplesFor limit = sumMultiples 0 (limit - 1)

sumMultiples sum limit
  | limit < 3     = sum
  | otherwise     = sumMultiples (newSum sum limit) (limit - 1)

newSum sum limit =
  if mod limit 3 == 0 || mod limit 5 == 0
    then sum + limit
    else sum

main = hspec $ do
  describe "MultipleSummer" $ do
    it "sums multiples of 3 & 5 up to but not including 3" $ do
      sumMultiplesFor 3 `shouldBe` 0

    it "sums multiples of 3 & 5 up to but not including 4" $ do
      sumMultiplesFor 4 `shouldBe` 3

    it "sums multiples of 3 & 5 up to but not including 5" $ do
      sumMultiplesFor 5 `shouldBe` 3

    it "sums multiples of 3 & 5 up to but not including 6" $ do
      sumMultiplesFor 6 `shouldBe` 8

    it "sums multiples of 3 & 5 up to but not including 7" $ do
      sumMultiplesFor 7 `shouldBe` 14

    it "sums multiples of 3 & 5 up to but not including 9" $ do
      sumMultiplesFor 9 `shouldBe` 14

    it "sums multiples of 3 & 5 up to but not including 10" $ do
      sumMultiplesFor 10 `shouldBe` 23

    it "sums multiples of 3 & 5 up to but not including 12" $ do
      sumMultiplesFor 12 `shouldBe` 33

    it "sums multiples of 3 & 5 up to but not including 13" $ do
      sumMultiplesFor 13 `shouldBe` 45

    it "sums multiples of 3 & 5 up to but not including 1000" $ do
      sumMultiplesFor 1000 `shouldBe` 233168
