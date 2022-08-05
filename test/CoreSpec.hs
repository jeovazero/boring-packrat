{-# Language OverloadedStrings #-}
module CoreSpec where

import Test.Hspec
import Data.ByteString
import BoringPackrat.Terminals
import BoringPackrat (
    parse,
    isNotParsed,
    isTotallyConsumed,
    isPartiallyConsumed,
    AST(..),
    PEG(..),
    Grammar,
    (#),
    ParsedResult(..),
    Result(..),
  )

astFrom :: ParsedResult -> AST
astFrom (PartiallyConsumed (Parsed _ ast _)) = ast
astFrom (TotallyConsumed (Parsed _ ast _)) = ast
astFrom _ = error "Not expect AST from NotParsed"

errorIndexFrom :: ParsedResult -> Int
errorIndexFrom (NotParsed i _) = i
errorIndexFrom _ = error "Not expect error index from Parsed"

{-

  == TotallyConsumed
  ~= PartiallyConsumed
  /= NotParsed

-}

parse' :: Grammar -> ByteString -> ParsedResult
parse' grammar = parse grammar "Grammar"

spec :: Spec 
spec = do
  describe "Many0 ::" $ do
    it "Many0 LitBS \"abc\" == 'abcabcabc'" $ do
      let result = parse' [("Grammar", Many0 $ litBS "abc")] "abcabcabc"
      
      result `shouldSatisfy` isTotallyConsumed
      astFrom result `shouldBe` Rule (0,8) "Grammar" (Seq (0,8) [Str "abc", Str "abc", Str "abc"])

    it "Many0 LitBS \"abc\" /= 'ABDabc'" $ do
      let result = parse' [("Grammar", Many0 $ litBS "abc")] "ABDabc"
      
      result `shouldSatisfy` isPartiallyConsumed
      astFrom result `shouldBe` Rule (0,-1) "Grammar" Void


  describe "Many1 ::" $ do
    it "Many1 HexDigit ~= 'fd0g'" $ do
      let result = parse' [("Grammar", Many1 _HexDigit)] "fd0g"
      
      result `shouldSatisfy` isPartiallyConsumed
      astFrom result `shouldBe` Rule (0,2) "Grammar" (Seq (0,2) [Str "f", Str "d", Str "0"])

    it "Many1 HexDigit /= 'Xfeed0g'" $ do
      let result = parse' [("Grammar", Many1 _HexDigit)] "Xfeed0g"
      
      result `shouldSatisfy` isNotParsed
      errorIndexFrom result `shouldBe` 0


  describe "Many ::" $ do
    it "Many (3,5) AlphaDigit == 'W4v3s'" $ do
      let result = parse' [("Grammar", Many (0,5) _AlphaDigit)] "W4v3s"
      
      result `shouldSatisfy` isTotallyConsumed
      astFrom result `shouldBe` Rule (0,4) "Grammar" (Seq (0,4) [Str "W", Str "4", Str "v", Str "3", Str "s"])

    it "Many (3,5) AlphaDigit ~= 'W4v~~'" $ do
      let result = parse' [("Grammar", Many (3,5) _AlphaDigit)] "W4v~~"
      
      result `shouldSatisfy` isPartiallyConsumed
      astFrom result `shouldBe` Rule (0,2) "Grammar" (Seq (0,2) [Str "W",Str "4",Str "v"])

    it "Many (3,5) AlphaDigit /= 'W4~~~'" $ do
      let result = parse' [("Grammar", Many (3,5) _AlphaDigit)] "W4~~"
      
      result `shouldSatisfy` isNotParsed
      errorIndexFrom result `shouldBe` 2


  describe "Repeat ::" $ do
    it "Repeat 3 HexDigit == 'baa'" $ do
      let result = parse' [("Grammar", Repeat 3 _HexDigit)] "baa"
      
      result `shouldSatisfy` isTotallyConsumed
      astFrom result `shouldBe` Rule (0,2) "Grammar" (Seq (0,2) [Str "b",Str "a",Str "a"])

    it "Repeat 3 HexDigit /= 'bag'" $ do
      let result = parse' [("Grammar", Repeat 3 _HexDigit)] "bag"
      
      result `shouldSatisfy` isNotParsed
      errorIndexFrom result `shouldBe` 2


  describe "ManyN ::" $ do
    it "ManyN 2 (LitBS \"hi\") ~= 'hihi-mister'" $ do
      let result = parse' [("Grammar", ManyN 2 $ litBS "hi")] "hihi-mister"
      
      result `shouldSatisfy` isPartiallyConsumed
      astFrom result `shouldBe` Rule (0,3) "Grammar" (Seq (0,3) [Str "hi",Str "hi"])
   
    it "ManyN 3 (LitBS \"hi\") /= 'hihi-mister'" $ do
      let result = parse' [("Grammar", ManyN 3 $ litBS "hi")] "hihi-mister"
      
      result `shouldSatisfy` isNotParsed
      errorIndexFrom result `shouldBe` 4


  describe "Cons ::" $ do
    it "Cons Alpha DigitDigit ~= 'a234'" $ do
      let result = parse' [("Grammar", _Alpha # Repeat 2 _Digit)] "a234"
      
      result `shouldSatisfy` isPartiallyConsumed
      astFrom result `shouldBe` Rule (0,2) "Grammar" (Cons' (0,2) (Str "a") (Seq (1,2) [Str "2",Str "3"]))

    it "Cons (Many1 Alpha) (Many0 Digit) == 'no'" $ do
      let result = parse' [("Grammar", Many1 _Alpha # Many0 _Digit)] "no"
      
      result `shouldSatisfy` isTotallyConsumed
      astFrom result
        `shouldBe`
           Rule (0,1) "Grammar" (Cons' (0,1) (Seq (0,1) [Str "n", Str "o"]) Void)

    it "Cons (Many1 Alpha) Alpha /= 'Nevermind'" $ do
      let result = parse' [("Grammar", Many1 _Alpha # _Alpha)] "Nevermind"
      
      result `shouldSatisfy` isNotParsed
      errorIndexFrom result `shouldBe` 9


  describe "Sequence ::" $ do
    it "Sequence [Alpha, DigitDigit] ~= 'a234'" $ do
      let result = parse' [("Grammar", Sequence [_Alpha,Repeat 2 _Digit])] "a234"
      
      result `shouldSatisfy` isPartiallyConsumed
      astFrom result `shouldBe` Rule (0,2) "Grammar" (Seq (0,2) [Str "a",Seq (1,2) [Str "2",Str "3"]])

    it "Sequence [Many1 Alpha,Many0 Digit] == 'yo'" $ do
      let result = parse' [("Grammar", Sequence [Many1 _Alpha,Many0 _Digit])] "yo"
      
      result `shouldSatisfy` isTotallyConsumed
      astFrom result
        `shouldBe`
           Rule (0,1) "Grammar" (Seq (0,1) [Seq (0,1) [Str "y", Str "o"],Void])

    it "Sequence [Many1 Alpha,Alpha] /= 'Nevermind'" $ do
      let result = parse' [("Grammar", Sequence [Many1 _Alpha,_Alpha])] "Nevermind"
      
      result `shouldSatisfy` isNotParsed
      errorIndexFrom result `shouldBe` 9


  describe "Choice ::" $ do
    it "Choice [Many1 Alpha, Many1 Digit] == 'ab20'" $ do
      let result = parse' [("Grammar", Choice [Many1 _Alpha,Many1 _Digit])] "ab20"

      result `shouldSatisfy` isPartiallyConsumed 
      astFrom result `shouldBe` Rule (0,1) "Grammar" (Seq (0,1) [Str "a", Str "b"])
    
    it "Choice [Many1 Alpha, Many1 Digit] == '123'" $ do
      let result = parse' [("Grammar", Choice [Many1 _Alpha,Many1 _Digit])] "123"

      result `shouldSatisfy` isTotallyConsumed
      astFrom result `shouldBe` Rule (0,2) "Grammar" (Seq (0,2) [Str "1", Str "2", Str "3"])

    it "Choice [Many1 Alpha, Many1 Digit] /= '---'" $ do
      let result = parse' [("Grammar", Choice [Many1 _Alpha,Many1 _Digit])] "---"

      result `shouldSatisfy` isNotParsed
      errorIndexFrom result `shouldBe` 0
  

  describe "Optional ::" $ do
    it "Optional (Many1 HexDigit) ~= ''" $ do
      let result = parse' [("Grammar", Optional (Many1 _HexDigit))] ""

      result `shouldSatisfy` isTotallyConsumed
      astFrom result `shouldBe` Rule (0,-1) "Grammar" Void

    it "Optional (Many1 HexDigit) ~= 'good'" $ do
      let result = parse' [("Grammar", Optional (Many1 _HexDigit))] "good"

      result `shouldSatisfy` isPartiallyConsumed
      astFrom result `shouldBe` Rule (0,-1) "Grammar" Void

    it "Optional (Many1 HexDigit) == 'fae'" $ do
      let result = parse' [("Grammar", Optional (Many1 _HexDigit))] "fae"

      result `shouldSatisfy` isTotallyConsumed
      astFrom result `shouldBe` Rule (0,2) "Grammar" (Seq (0,2) [Str "f", Str "a", Str "e"])


  describe "And ::" $ do
    it "And (Many1 Digit) # Many0 Digit /= 'abc'" $ do
      let result = parse' [("Grammar", And (Many1 _Digit) # Many0 _Digit)] "abc"

      result `shouldSatisfy` isNotParsed
      errorIndexFrom result `shouldBe` 0

    it "And (Many1 Digit) # Many0 Digit == '123'" $ do
      let result = parse' [("Grammar", And (Many1 _Digit) # Many0 _Digit)] "123"

      result `shouldSatisfy` isTotallyConsumed
      astFrom result `shouldBe` Rule (0,2) "Grammar" (Cons' (0,2) Void (Seq (0,2) [Str "1", Str "2", Str "3"]))


  describe "Not ::" $ do
    it "Not (Many1 Digit) # Many0 Alpha /= '1abc'" $ do
      let result = parse' [("Grammar", Not (Many1 _Digit) # Many0 _Alpha)] "1abc"

      result `shouldSatisfy` isNotParsed
      errorIndexFrom result `shouldBe` 0

    it "Not (Many1 Digit) # Many0 Alpha == 'abc'" $ do
      let result = parse' [("Grammar", Not (Many1 _Digit) # Many0 _Alpha)] "abc"

      result `shouldSatisfy` isTotallyConsumed
      astFrom result `shouldBe` Rule (0,2) "Grammar" (Cons' (0,2) Void (Seq (0,2) [Str "a", Str "b", Str "c"]))
 
