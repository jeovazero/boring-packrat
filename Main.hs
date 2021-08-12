{-# LANGUAGE OverloadedStrings #-}
import BoringPackrat (parse, Terminal'(..), PEG(..), (#), RuleName, ParsedResult(..), Result(..), Layer(..))
import qualified Data.Word8 as W
import qualified Data.Vector as V

printParsed input (Parsed range ast _) = do
  putStrLn $ show input
  putStrLn "== Range =="
  putStrLn $ show range
  putStrLn "== AST =="
  putStrLn $ show ast

printResults nonTerms a = do
  case parse nonTerms a of
    AllParsed x -> do
      putStrLn "All parsed"
      printParsed a x
    PartialParsed x -> do
      putStrLn "Partial parsed"
      printParsed a x
    NotParsed -> putStrLn "Not Parsed"

helloNT =
  [ ("G", Sequence [NonTerminal "A", Terminal SP, NonTerminal "B"])
  , ("A", Terminal $ LitBS "hello")
  , ("B", Terminal $ LitBS "friend")
  ] :: [(RuleName,PEG)]

nonTerms =
  [ ("Add", Choice [NonTerminal "Mult" # Terminal (Lit W._plus) # NonTerminal "Add", NonTerminal "Mult"])
  , ("Mult", Choice [NonTerminal "Prim" # Terminal (Lit W._asterisk) # NonTerminal "Mult", NonTerminal "Prim"])
  , ("Prim", Choice [Terminal (Lit W._parenleft) # NonTerminal "Add" # Terminal (Lit W._parenright), NonTerminal "Dec"])
  , ("Dec", Terminal Digit)
  ] :: [(RuleName,PEG)]

main = do
  -- let a = "(1+1)+(2*(2+1*(3*5+(9+1*(3*4*(1*3+(2+1*(1+6*(1*(1+(1*(2*3+(1+3*(3+3)))))))))))))))"
  let a = "1+1)"
  -- let a = "(0+1+2+3)"
  printResults nonTerms a

  let b = "hello friend"
  printResults helloNT b

