{-# Language OverloadedStrings #-}
import Test.Hspec
import Data.Word8 as W
import Data.ByteString
import BoringPEG (decodePEG, PEG(..))

consumePEG peg str =
    case decodePEG (peg, unpack str) of
        Right (_,rest) -> Just $ pack rest
        Left _ -> Nothing

main = hspec $ do
    describe "Sequence" $ do
        it "sequence of literals" $ do
            consumePEG (Sequence (Lit W._a) (Lit W._b)) "abc" `shouldBe` Just "c"
        it "sequence of digits" $ do
            consumePEG (Sequence Digit Digit) "454bc" `shouldBe` Just "4bc"
