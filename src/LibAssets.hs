module LibAssets
    ( getRecords
    , fBt
    , fBd
    , fBo
    , fTotal
    , fAvg
    , r2L
    , s2L
    , saveRegs2File
    , someFunc
    ) where

import Filters ( avg
               , total
               , filterByTicker
               , filterBetweenDates
               , filterByOpType
               )
import DataTypes ( Register (..)
                 , OpType (..)
                 , Ticker
                 , Summary
                 )
import Reports ( reg2Lists
               , summ2Lists
               )
import System.IO ()
import Data.Time ( Day )

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

s2L :: Functor f => f Summary -> f [String]
s2L = fmap summ2Lists

r2L :: Functor f => f [Register] -> f [String]
r2L = fmap reg2Lists

saveRegs2File :: FilePath -> IO [String] -> IO ()
saveRegs2File path r2l = fmap unlines r2l >>= writeFile path

fBo :: Functor f => String -> f [Register] -> f [Register]
fBo op = fmap (\x -> filterByOpType op x)

fBd :: Functor f => String -> f [Register] -> f [Register]
fBd dts = fmap (\x -> filterBetweenDates dts x)

fBt :: Functor f => String -> f [Register] -> f [Register]
fBt tk = fmap (\x -> filterByTicker tk x)

fTotal :: Functor f => f [Register] -> f (Double, Int)
fTotal = fmap total

fAvg :: Functor f => f [Register] -> f (Double, Int)
fAvg = fmap avg

someFunc :: IO ()
someFunc = putStrLn "someFunc"
