module Lib
    ( getRecords
    , fTotal
    , fReg
    , someFunc
    ) where

import System.IO ()
import Data.Time ()
import Data.Time.Format ()

type Ticker = String
data OpType = COMPRA | VENDA deriving (Show)

data Register  = Register { ticker   :: String
                          , date     :: String
                          , opType   :: String
                          , quantity :: Int
                          , value    :: Double
                          } deriving (Show)

getRecords :: FilePath -> IO [Register]
getRecords path = do
   contents <- readFile path 
   let lns = lines contents
   let ws = map words lns
   let regs = map (\x -> Register (x !! 2) (head x) (x !! 1)
                                    (read (x !! 3) :: Int)
                                    (read (x !! 4) :: Double)) ws
   return regs

filterByTicker :: String -> [Register] -> [Register]
filterByTicker "all" regs = regs
filterByTicker tk    regs = filter (\reg -> ticker reg == tk) regs

fReg :: Functor f => String -> f [Register] -> f [Register]
fReg tk = fmap (\x -> filterByTicker tk x)

total :: [Register] -> Double
total = foldr (\reg acc -> acc + fromIntegral (quantity reg) * value reg) 0

fTotal :: Functor f => f [Register] -> f Double
fTotal = fmap total

someFunc :: IO ()
someFunc = putStrLn "someFunc"
