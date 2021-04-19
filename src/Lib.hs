module Lib
    ( getRecords
    , fTotal
    , fReg
    , fbd
    , someFunc
    ) where

import System.IO ()
import Data.Time (Day)
import Data.Time.Format ()

type Ticker = String
data OpType = COMPRA | VENDA deriving (Show, Read)

data Register  = Register { ticker   :: Ticker 
                          , date     :: Day
                          , opType   :: OpType 
                          , quantity :: Int
                          , value    :: Double
                          } deriving (Show)

getRecords :: FilePath -> IO [Register]
getRecords path = do
   contents <- readFile path 
   let lns = lines contents
   let ws = map words lns
   let regs = map (\x -> Register (x !! 2) 
                                  (read (head x) :: Day) 
                                  (read (x !! 1) :: OpType)
                                  (read (x !! 3) :: Int)
                                  (read (x !! 4) :: Double)) ws
   return regs

filterBetweenDates :: String -> [Register] -> [ Register]
filterBetweenDates "" regs  = regs
filterBetweenDates dts regs = filter (\reg -> elem (date reg) range) regs
                                where [dI, dF] = words dts
                                      range = [(read dI::Day) .. (read dF::Day)]

fbd :: Functor f => String -> f [Register] -> f [Register]
fbd dts = fmap (\x -> filterBetweenDates dts x)

filterByTicker :: String -> [Register] -> [Register]
filterByTicker "all" regs = regs
filterByTicker tk    regs = filter (\reg -> elem (ticker reg) $ words tk) regs

fReg :: Functor f => String -> f [Register] -> f [Register]
fReg tk = fmap (\x -> filterByTicker tk x)

total :: [Register] -> Double
total = foldr (\reg acc -> acc + fromIntegral (quantity reg) * value reg) 0

fTotal :: Functor f => f [Register] -> f Double
fTotal = fmap total

someFunc :: IO ()
someFunc = putStrLn "someFunc"
