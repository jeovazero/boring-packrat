{-# Language OverloadedStrings #-}
import Test.Hspec
import Data.ByteString.Char8 as B8
import BoringPackrat (
    parse,
--  prettyPrint,
    isAllParsed,
    PEG(..),
    Grammar,
    ParsedResult(..),
  )
import BoringPackrat.EmailGrammar (emailGrammar)

parse' :: Grammar -> ByteString -> ParsedResult
parse' grammar = parse grammar "Email"

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
  , "user.name+tag+sorting@example.com" -- (may go to user.name@example.com inbox depending on mail server)
  , "x@example.com" -- (one-letter local-part)
  , "example-indeed@strange-example.com"
  , "test/test@test.com" -- (slashes are a printable character, and allowed)
  , "admin@mailserver1" -- (local domain name with no TLD, although ICANN highly discourages dotless email addresses[10])
  , "example@s.example" -- (see the List of Internet top-level domains)
  , "\" \"@example.org" -- (space between the quotes)
  , "\"john..doe\"@example.org" -- (quoted double dot)
  , "mailhost!username@example.org" -- (bangified host route used for uucp mailers)
  , "user%example.com@example.org" -- (% escaped mail route to user@example.com via example.org)
  , "user-@example.org" -- (local part ending with non-alphanumeric character from the list of allowed printable characters)
  ]

invalids =
  [ "email@-example.com"
  , "Abc.example.com" -- (no @ character)
  , "A@b@c@example.com" -- (only one @ is allowed outside quotation marks)
  , "a\"b(c)d,e:f;g<h>i[j\\k]l@example.com" -- (none of the special characters in this local-part are allowed outside quotation marks)
  , "just\"not\"right@example.com" -- (quoted strings must be dot separated or the only element making up the local-part)
  , "this is\"not\\allowed@example.com" -- (spaces, quotes, and backslashes may only exist when within quoted strings and preceded by a backslash)
  , "this\\ still\\\"not\\\\allowed@example.com" -- (even if escaped (preceded by a backslash), spaces, quotes, and backslashes must still be contained by quotes)
  --, "1234567890123456789012345678901234567890123456789012345678901234+x@example.com" -- (local-part is longer than 64 characters)
  , "i_like_underscore@but_its_not_allowed_in_this_part.example.com" -- (Underscore is not allowed in domain part)
  , "QA[icon]CHOCOLATE[icon]@test.com" -- (icon characters)
  ]

-- TODO: 
--
-- From RFC 5321
--   4.5.3.1.1. Local-part
--     The maximum total length of a user name or other local-part is 64
--     octets.
--
--   4.5.3.1.2. Domain
--     The maximum total length of a domain name or number is 255 octets.

main :: IO ()
main = hspec $ do
  describe "Valid Emails" $ do
   mapM_ (
      \valid -> do
        it (B8.unpack valid) $ do
          let result = parse' emailGrammar valid
          result `shouldSatisfy` isAllParsed
    ) valids

  describe "Invalid Emails" $ do
   mapM_ (
      \invalid -> do
        it (B8.unpack invalid) $ do
          let result = parse' emailGrammar invalid
          result `shouldNotSatisfy` isAllParsed
    ) invalids

