{-# LANGUAGE OverloadedStrings #-}
import BoringPEG (decodePeg, _Email, Peg(..), (#))
import qualified Data.Word8 as W
import Data.ByteString


printPeg peg str = do
  print
    $ fmap (\(x,y) -> (pack y))
    $ decodePeg (peg, unpack str)

main = do
  printPeg _Email "foo-ze-ze@ze-.com"
  printPeg (Lit W._a # SP # Lit W._j) "a joker"
  printPeg (HexDigit # HexDigit # HexDigit) "aB232"
  printPeg (Not Digit # HexDigit # HexDigit) "aB232"

