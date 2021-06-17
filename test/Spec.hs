import Test.Hspec

main = hspec $ do
    describe "2 + 2" $ do
        it "adds 2 + 2 to equal 4" $
            2 + 2 `shouldBe` 4
