{-# LANGUAGE OverloadedStrings #-}
import BoringPEG (decodePEG, PEG(..), (#))
import BoringPEG.EmailSpec (_Email)
import qualified Data.Word8 as W
import Data.ByteString


printPEG peg str = do
  print
    $ fmap (\(x,y) -> (pack y))
    $ decodePEG (peg, unpack str)

main = do
  printPEG _Email "foo-ze-ze@ze-.com"
  printPEG (Lit W._a # SP # Lit W._j) "a joker"
  printPEG (HexDigit # HexDigit # HexDigit) "aB232"
  printPEG (Not Digit # HexDigit # HexDigit) "aB232"

