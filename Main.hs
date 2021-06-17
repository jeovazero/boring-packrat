{-# LANGUAGE OverloadedStrings #-}
import BoringPEG (decodeFoo, _Email, Foo(..), (#))
import qualified Data.Word8 as W
import Data.ByteString


printFoo peg str = do
  print
    $ fmap (\(x,y) -> (pack y))
    $ decodeFoo (peg, unpack str)

main = do
  printFoo _Email "foo-ze-ze@ze-.com"
  printFoo (Lit W._a # SP # Lit W._j) "a joker"
  printFoo (HexDigit # HexDigit # HexDigit) "aB232"
  printFoo (Not Digit # HexDigit # HexDigit) "aB232"

