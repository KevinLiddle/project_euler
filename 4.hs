import Test.Hspec

largestPalindrome = largestPalindromeRecur 999 999 0

largestPalindromeRecur first second best
  | first < 100 && second < 100        = best
  | first < 100 && second >= 100       = largestPalindromeRecur (second - 1) (second - 1) best
  | betterPalindrome first second best = largestPalindromeRecur (first - 1) second (first * second)
  | otherwise                          = largestPalindromeRecur (first - 1) second best

betterPalindrome first second best =
  palindrome (first * second) && first * second > best

palindrome number = show number == (reverse $ show number)

main = hspec $ do
  describe "#palindrome" $ do
    it "knows that 101 is a palindrome" $ do
      palindrome 101 `shouldBe` True

    it "knows that 100 is not a palindrome" $ do
      palindrome 100 `shouldBe` False

    it "knows that 1001 is a palindrome" $ do
      palindrome 1001 `shouldBe` True

  describe "#largestPalindrome" $ do
    it "finds the largest palindrome from the product of two 3-digit numbers" $ do
      largestPalindrome `shouldBe` 906609
