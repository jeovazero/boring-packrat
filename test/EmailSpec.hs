{-# LANGUAGE OverloadedStrings #-}
module EmailSpec where

import Test.Hspec
import Data.ByteString.Char8 as B8
import BoringPackrat.Email (checkEmail,parseEmail)

-- Email addresses from https://en.wikipedia.org/wiki/Email_address
valids, invalids :: [ByteString]
valids =
  [ "firstname.lastname@example.com"
  , "email@example.co.jp"
  , "simple@example.com"
  , "very.common@example.com"
  , "disposable.style.email.with+symbol@example.com"
  , "other.email-with-hyphen@example.com"
  , "fully-qualified-domain@example.com"
  , "user.name+tag+sorting@example.com"
  , "x@example.com"
  , "example-indeed@strange-example.com"
  , "test/test@test.com"
  , "admin@mailserver1"
  , "example@s.example"
  , "\" \"@example.org"
  , "\"john..doe\"@example.org"
  , "mailhost!username@example.org"
  , "user%example.com@example.org"
  , "user-@example.org"
  ]

invalids =
  [ "email@-example.com"
  , "Abc.example.com"
  , "A@b@c@example.com"
  , "a\"b(c)d,e:f;g<h>i[j\\k]l@example.com"
  , "just\"not\"right@example.com"
  , "this is\"not\\allowed@example.com"
  , "this\\ still\\\"not\\\\allowed@example.com"
  , "1234567890123456789012345678901234567890123456789012345678901234+x@example.com"
  , "i_like_underscore@but_its_not_allowed_in_this_part.example.com"
  , "QA[icon]CHOCOLATE[icon]@test.com"
  ]

spec :: Spec
spec = do
  describe "Valid Emails" $
   mapM_ (
      \valid ->
        it (B8.unpack valid) $ do
          print $ parseEmail valid
          valid `shouldSatisfy` checkEmail 
    ) valids

  describe "Invalid Emails" $
   mapM_ (
      \invalid ->
        it (B8.unpack invalid) $ invalid `shouldNotSatisfy` checkEmail
    ) invalids

