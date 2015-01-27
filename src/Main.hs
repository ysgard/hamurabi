{-|
Module : Hammurabi
Description: A port of the BASIC computer game into Haskell
Copyright: (c) Jan Van Uytven, 2015
License: MIT
Maintainer : ysgard@gmail.com
Stability: experimental
Portability: POSIX

-}
module Main where

import System.IO (stdout, hFlush)

data Kingdom = Kingdom { year :: Int
                       , people :: Int
                       , acres :: Int
                       , bushels :: Int
                       , lost :: Int
                       } deriving (Show)


intro :: IO ()
intro = do
  putStrLn "                 HAMMURABI"
  putStrLn "CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY"
  putStrLn "     Haskell port by Jan Van Uytven"
  putStrLn ""
  putStrLn "TRY YOUR HAND AT GOVERNING ANCIENT SUMERIA"
  putStrLn "FOR A TEN-YEAR TERM OF OFFICE."
  putStrLn ""



main :: IO ()
main = do
  intro
