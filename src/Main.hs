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

import System.Exit (exitSuccess)
import System.IO (stdout, hFlush)
import System.Random (getStdRandom, random, randomR)
import Text.Read (readMaybe)

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

end :: IO ()
end = putStrLn "" >> putStrLn "SO LONG FOR NOW."


stewardMad :: IO ()
stewardMad = do
  putStrLn ""
  putStrLn "HAMURABI:  I CANNOT DO WHAT YOU WISH."
  putStrLn "GET YOURSELF ANOTHER STEWARD!!!!!"
  end

tallyScore :: Int -> Int -> Score -> IO ()
tallyScore pop a (Score d p) = do
  putStrLn $ "IN YOUR 10-YEAR TERM OF OFFICE, " ++ show p ++ " PERCENT OF THE"
  putStrLn "POPULATION STARVED PER YEAR ON THE AVERAGE, I.E. A TOTAL OF"
  putStrLn $ show d ++ " PEOPLE DIED!!"
  let efficiency = a `div` pop
  putStrLn ""
  putStrLn "YOU STARTED WITH 10 ACRES PER PERSON AND ENDED WITH"
  putStrLn $ show efficiency ++ " ACRES PER PERSON."
  putStrLn ""
  performance p efficiency d pop

performance :: Double -> Int -> Int -> Int -> IO ()
performance p e d pop
      | p > 33.0 || e < 7 = oops d
      | p > 10.0 || e < 9 = do
          putStrLn "YOUR HEAVY HANDED PERFORMANCE SMACKS OF NERO AND IVAN IV."
          putStrLn "THE PEOPLE (REMAINING) FIND YOU AN UNPLEASANT RULER, AND,"
          putStrLn "FRANKLY, HATE YOUR GUTS!!"
      | p > 3.0 || e < 10 = do
          c <- getStdRandom random :: IO Double
          let a = floor $ (fromIntegral pop) * 0.8 * c :: Int
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
  putStrLn ""
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
loop k@(Kingdom y _ pop agri) s
  | y == 11 = tallyScore (people pop) (acres agri) s
  | otherwise = do
      report k
      newKingdom <- updateKingdom k
      let newScore = updateScore s newKingdom
      loop newKingdom newScore
      

-- Get input
ask :: String -> IO Int
ask prompt = do
  putStr $ prompt ++ " "
  hFlush stdout
  a <- getLine
  case ((readMaybe a) :: Maybe Int) of
   Nothing -> putStrLn "INVALID INPUT" >> ask prompt
   Just n -> case (n < 0) of
              True -> do
                stewardMad
                exitSuccess
              False -> return n

updateKingdom :: Kingdom -> IO Kingdom
updateKingdom k = unsetPlague k >>=
                  buySellAcres  >>=
                  feedPeople    >>=
                  plantAcres    >>=
                  immigration   >>=
                  gotPlague     >>=
                  updateYear
  
updateYear :: Kingdom -> IO Kingdom
updateYear (Kingdom y p pop agri) = return (Kingdom (y + 1) p pop agri)

unsetPlague :: Kingdom -> IO Kingdom
unsetPlague (Kingdom y _ pop agri) = return (Kingdom y False pop agri)

gotPlague :: Kingdom -> IO Kingdom
gotPlague k@(Kingdom y _ (Population p n s) agri) = do
  pc <- percentChance 15
  if pc
    then return (Kingdom y True (Population (p `div` 2) n s) agri)
    else return k

immigration :: Kingdom -> IO Kingdom
immigration (Kingdom y pl (Population p _ s) agri@(Agriculture b a _ _)) = do
  c <- getStdRandom $ randomR (1, 5) :: IO Int
  let cd = fromIntegral c :: Double
      ad = fromIntegral a :: Double
      bd = fromIntegral b :: Double
      pd = fromIntegral p :: Double
      i = floor $ 1 + (cd * (20 * ad + bd) / (pd * 100)) :: Int
  return (Kingdom y pl (Population (p + i) i s) agri)

plantAcres :: Kingdom -> IO Kingdom
plantAcres k@(Kingdom y p pop (Agriculture b a _ _)) = do
  planted <- ask "HOW MANY ACRES DO YOU WISH TO PLANT WITH SEED?"
  check planted
  letsPlant planted
  where letsPlant :: Int -> IO Kingdom
        letsPlant x
          | x > a = do
              notEnoughAcres a
              plantAcres k
          | x `div` 2 > b = do
              notEnoughGrain b
              plantAcres k
          | x > 10 * (people pop) = do
              notEnoughPeople (people pop)
              plantAcres k
          | otherwise = do
              -- bountiful harvest
              i <- getStdRandom $ randomR (1, 5) :: IO Int
              let r = if even i then b `div` i else 0
              return (Kingdom y p pop
                      (Agriculture (b + x*i - r) a (x*i) r)) 
              
              

buySellAcres :: Kingdom -> IO Kingdom
buySellAcres (Kingdom y p pop (Agriculture b a h r)) = do
  landPrice <- getStdRandom $ randomR (17, 27) :: IO Int
  putStrLn $ "LAND IS TRADING AT " ++ show landPrice ++ " BUSHELS PER ACRE."
  acresBought <- buyAcres landPrice b
  if acresBought > 0
    then return (Kingdom y p pop (Agriculture (b - (acresBought * landPrice))
                                  (a + acresBought)
                                  h r))
    else do acresSold <- sellAcres landPrice a
            return (Kingdom y p pop (Agriculture (b + (acresSold * landPrice))
                                      (a - acresSold)
                                      h r))

notEnoughGrain :: Int -> IO ()
notEnoughGrain x = do
  putStrLn "HAMURABI:  THINK AGAIN.  YOU HAVE ONLY "
  putStrLn $ show x ++ " BUSHELS OF GRAIN.  NOW THEN,"

notEnoughAcres :: Int -> IO ()
notEnoughAcres x = do
  putStrLn "HAMURABI:  THINK AGAIN.  YOU OWN ONLY "
  putStrLn $ show x ++ " ACRES OF LAND.  NOW THEN,"

notEnoughPeople :: Int -> IO ()
notEnoughPeople x = do
  putStrLn $ "HAMURABI:  BUT YOU HAVE ONLY " ++ show x
  putStrLn "PEOPLE TO TEND THE FIELDS!  NOW THEN,"
  
              
-- Each person needs 20 bushels a year for food.
feedPeople :: Kingdom -> IO Kingdom
feedPeople k@(Kingdom y pl (Population p n _) (Agriculture b a h r)) = do
  food <- ask "HOW MANY BUSHELS DO YOU WISH TO FEED YOUR PEOPLE?"
  check food
  if food > b
    then do notEnoughGrain b
            feedPeople k
    else do let stv = checkStarvation food p
            if stv > (ceiling (0.45 * (fromIntegral p :: Double)) :: Int)
              then do oops stv
                      end
                      exitSuccess
              else return (Kingdom y pl (Population (p - stv) n stv)
                           (Agriculture (b - food) a h r))
  where checkStarvation :: Int -> Int -> Int
        checkStarvation fud ppl
          | fud < ppl * 20 = (ppl * 20 - fud) `div` 20
          | otherwise = 0

buyAcres :: Int -> Int -> IO Int
buyAcres pricePerAcre b = do
  acresBought <- ask "HOW MANY ACRES DO YOU WISH TO BUY?"
  check acresBought
  if acresBought * pricePerAcre <= b
    then return acresBought
    else do notEnoughGrain b
            buyAcres pricePerAcre b
  
sellAcres :: Int -> Int -> IO Int
sellAcres pricePerAcre a = do
  acresSold <- ask "HOM MANY ACRES DO YOU WISH TO SELL"
  check acresSold
  if acresSold <= a
    then return acresSold
    else do notEnoughAcres a
            sellAcres pricePerAcre a

-- Check for <0 in input
check :: Int -> IO ()
check n
  | n < 0 = do
      stewardMad
      exitSuccess
  | otherwise = return ()



updateScore :: Score -> Kingdom -> Score
updateScore (Score d pct) (Kingdom y pl (Population p _ s) _) =
  (Score (d + pox + s) newpct)
  where pox = if pl then p else 0
        newpct = (starvedPct + (pct * dpy)) / dy
          where sd = fromIntegral s :: Double
                pd = fromIntegral p :: Double
                dpy = fromIntegral (y - 1) :: Double
                dy = fromIntegral y :: Double
                starvedPct = (sd / pd) * 100 

-- -- Takes a percentage, returns true if it happened, false otherwise.
percentChance :: Int -> IO Bool
percentChance n = do
  r <- getStdRandom $ randomR (1, 100) :: IO Int -- Return random number from 1 to 100
  return $ r <= n


main :: IO ()
main = do
  intro
  let initial = Kingdom 1 False (Population 100 5 0) (Agriculture 2800 1000 3 200)
      score = Score 0 0
  loop initial score
  end
