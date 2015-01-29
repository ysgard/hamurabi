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
import System.Random (getStdRandom, random)

data Population = Population { people :: Int -- total population
                             , new :: Int -- immigration this year
                             , starved :: Int -- starved this year
                             } deriving (Show)

data Score = Score { died :: Int -- death toll so far
                   , percentage :: Double -- average percentage of deaths
                   } deriving (Show)

data Agriculture = Agriculture { bushels :: Int -- total bushels
                               , acres :: Int -- total acres
                               , harvested :: Int -- bushels harvested per acre
                               , rats :: Int -- bushels eaten by rats
                               } deriving (Show)

data Kingdom = Kingdom { year :: Int -- current year
                       , plague :: Bool  -- was there a plague this year?
                       , population :: Population 
                       , agriculture :: Agriculture
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

tallyScore :: Int -> Int -> Score -> IO ()
tallyScore pop acres (Score died percentage) = do
  putStrLn $ "IN YOUR 10-YEAR TERM OF OFFICE, " ++ show percentage ++
    " PERCENT OF THE"
  putStrLn "POPULATION STARVED PER YEAR ON THE AVERAGE, I.E. A TOTAL OF"
  putStrLn $ show died ++ " PEOPLE DIED!!"
  let efficiency = acres `div` pop
  putStrLn ""
  putStrLn "YOU STARTED WITH 10 ACRES PER PERSON AND ENDED WITH"
  putStrLn $ show efficiency ++ " ACRES PER PERSON."
  putStrLn ""
  performance percentage efficiency died pop

performance :: Double -> Int -> Int -> Int -> IO ()
performance p e d pop
      | p > 33.0 || e < 7 = oops d
      | p > 10.0 || e < 9 = do
          putStrLn "YOUR HEAVY HANDED PERFORMANCE SMACKS OF NERO AND IVAN IV."
          putStrLn "THE PEOPLE (REMAINING) FIND YOU AN UNPLEASANT RULER, AND,"
          putStrLn "FRANKLY, HATE YOUR GUTS!!"
      | p > 3.0 || e < 10 = do
          c <- getStdRandom random :: IO Double
          let a = floor $ (fromIntegral pop) * 0.8 * c
          putStrLn "YOUR PERFORMANCE COULD HAVE BEEN SOMEWHAT BETTER, BUT"
          putStrLn $ "REALLY WASN'T TOO BAD AT ALL.  " ++ show a ++ " PEOPLE"
          putStrLn "DEARLY LIKE TO SEE YOU ASSASSINATED BUT WE ALL HAVE OUR"
          putStrLn "TRIVIAL PROBLEMS."
      | otherwise = do
          putStrLn "A FANTASTIC PERFORMANCE!!!  CHARLEMAGNE, DISREALI, AND"
          putStrLn "JEFFERSON COMBINED COULD NOT HAVE DONE BETTER!"




oops :: Int -> IO ()
oops dead = do
  putStrLn $ "YOU STARVED " ++ show dead ++ " PEOPLE IN ONE YEAR!!!"
  putStrLn "DUE TO THIS EXTREME MISMANAGEMENT YOU HAVE NOT ONLY"
  putStrLn "BEEN IMPEACHED AND THROWN OUT OF OFFICE BUT YOU HAVE"
  putStrLn "ALSO BEEN DECLARED NATIONAL FINK!!!!"
  putStrLn ""

report :: Kingdom -> IO ()
report (Kingdom y p pop agri) = do
  putStrLn "HAMURABI:  I BEG TO REPORT TO YOU,"
  putStrLn $ "IN YEAR " ++ show y ++ " , " ++
    show (starved pop) ++ " PEOPLE STARVED, " ++
    show (new pop) ++ " CAME TO THE CITY,"
  if p
    then putStrLn "A HORRIBLE PLAGUE STRUCK!  HALF THE PEOPLE DIED."
    else return ()
  putStrLn $ "POPULATION IS NOW " ++ show (people pop)
  putStrLn $ "THE CITY NOW OWNS " ++ show (acres agri) ++ " ACRES."
  putStrLn $ "YOU HARVESTED " ++ show (harvested agri) ++ " BUSHELS PER ACRE."
  putStrLn $ "RATS ATE " ++ show (rats agri) ++ " BUSHELS."
  putStrLn $ "YOU NOW HAVE " ++ show (bushels agri) ++ " IN STORE."
  putStrLn ""

loop :: Kingdom -> Score -> IO ()
loop k@(Kingdom y p pop agri) s
  | y == 11 = tallyScore (people pop) (acres agri) s
  | otherwise = do
      report k
      newKingdom <- updateKingdom k
      let newScore = updateScore s newKingdom
      loop newKingdom newScore
      

updateKingdom :: Kingdom -> IO Kingdom
updateKingdom k@(Kingdom y p pop agri) = undefined
  
                                 

updateScore :: Score -> Kingdom -> Score
updateScore s k = undefined


main :: IO ()
main = do
  intro
  let initial = Kingdom 1 False (Population 100 5 0) (Agriculture 2800 1000 3 200)
      score = Score 0 0
  loop initial score
  putStrLn ""
  putStrLn "SO LONG FOR NOW."
