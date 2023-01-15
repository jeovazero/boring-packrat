{-# LANGUAGE OverloadedStrings #-}
import BoringPackrat (
    parse,
    astFrom
  )
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromJust)
import Text.Pretty.Simple (pPrint)
import Parse
import Grammar

-- import BoringPackrat.PrettyPrint (prettyPrint)
--

program input = do
  putStrLn $ B8.unpack input
  let result = parse grammar "program" input
--  print $ result
  let ast = fromJust $ astFrom result
--  prettyPrint input ast
  let s = makeSubtr input
  pPrint $ transformAST s ast

main = do
  let input1 =
        B8.unlines
            [ "fat n = fat' n 1",
            "fat' a acc = acc",
            "fat' n acc = fat' (n - 1) (n * acc)",
            "r = let x = 1 + 2, y = 2 in x - y + x - y + x + x",
             "x = case z | True -> a b | False -> b + 2",
             "x = a b",
             "x = guard | x == a + b -> c + 2 + c | x == 2 -> 5",
             "data Result a b = Ok a | Err b",
             "r = Ok 5",
             "e = Err NotFound",
            "lam = \\x -> \\y -> x + y"
            ]

  {-
  let input2 =
        B8.unlines
            [ "f x = x"
            , "g a b = a + b"
            , "fat n = fat' n 1"
            , "fat' 1 acc = acc"
            , "fat' n acc = fat' (n - 1) (n * acc)"
            , "expr a b = g 1 b - a * f 8"
            , "run (fat 5 + expr 4 9)"
            ]

  -- TODO: grammar should accept this one
  let input3 = "run (a + f (b * (c - d) + 5 - (8 - 7)))"
  -}
  program input1
  -- program input2
