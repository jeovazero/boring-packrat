{-# LANGUAGE OverloadedStrings #-}
module BoringPackrat.PrettyPrint (prettyPrint) where 

import qualified Data.ByteString.Char8 as B8
import Prelude as P
import BoringPackrat (AST(..),substr)

applyIdent :: Int -> IO ()
applyIdent ident = B8.putStr $ B8.replicate ident ' '

loopList :: B8.ByteString -> B8.ByteString -> Int -> [AST] -> IO ()
loopList input prefix ident (ast:xs) = do
  applyIdent ident
  B8.putStr prefix
  prettyPrint' (ident + 2) True input ast
  loopList input prefix ident xs
loopList _ _ _ [] = pure ()

prettyPrint :: B8.ByteString -> AST -> IO ()
prettyPrint = prettyPrint' 0 False

prettyPrint' :: Int -> Bool -> B8.ByteString -> AST -> IO ()
prettyPrint' ident ignoreIdent input ast =
  let
    applyIdent' = if ignoreIdent then pure () else applyIdent ident
    padding = if ignoreIdent then 0 else 2
    showInfo range = do
      P.putStr $ show range ++ " -> "
      P.putStr "\""
      B8.putStr $ substr range input
      P.putStrLn "\""
  in
  case ast of
    Seq range asts -> do
      applyIdent'
      P.putStr "Sequence -> "
      showInfo range
      loopList input "-" (ident + padding) asts
    Rule name range ast' -> do
      applyIdent'
      B8.putStr $ B8.concat ["Rule ",name," -> "]
      showInfo range
      prettyPrint' (ident + padding) False input ast'
    Str range -> do
      applyIdent'
      showInfo range
    Void -> do
      P.putStrLn "*"

