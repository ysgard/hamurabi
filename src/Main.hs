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
                       , starved :: Int
                       , new :: Int
                       , acres :: Int
                       , bushels :: Int
                       , harvested :: Int
                       , lost :: Int
                       } deriving (Show)


intro :: IO ()
intro = do
  putStrLn "                 HAMURABI"
  putStrLn "CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY"
  putStrLn "     Haskell port by Jan Van Uytven"
  putStrLn ""
  putStrLn "TRY YOUR HAND AT GOVERNING ANCIENT SUMERIA"
  putStrLn "FOR A TEN-YEAR TERM OF OFFICE."
  putStrLn ""

report :: Kingdom -> IO ()
report k = do
  putStrLn "HAMURABI:  I BEG TO REPORT TO YOU,"
  putStrLn $ "IN YEAR " ++ show (year k) ++ " , " ++
    show (starved k) ++ " PEOPLE STARVED, " ++
    show (new k) ++ " CAME TO THE CITY,"
  putStrLn $ "POPULATION IS NOW " ++ show (people k)
  putStrLn $ "THE CITY NOW OWNS " ++ show (acres k) ++ " ACRES."
  putStrLn $ "YOU HARVESTED " ++ show (harvested k) ++ " BUSHELS PER ACRE."
  putStrLn $ "YOU NOW HAVE " ++ show (bushels k) ++ "IN STORE."
  putStrLn ""


main :: IO ()
main = do
  intro
