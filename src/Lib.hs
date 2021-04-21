module Lib
    ( getRecords
    , fBt
    , fBd
    , fBo
    , fTotal
    , fAvg
    , someFunc
    , OpType(..)
    , Register(..)
    ) where

import System.IO ()
import Data.Time (Day)

type Ticker = String
data OpType = COMPRA | VENDA deriving (Show, Read, Eq)

data Register  = Register { ticker   :: Ticker 
                          , date     :: Day
                          , opType   :: OpType 
                          , quantity :: Int
                          , value    :: Double
                          } deriving (Show, Eq)

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

filterByOpType :: String -> [Register] -> [Register]
filterByOpType "" regs = regs
filterByOpType op regs = filter (\reg -> opType reg == (read op::OpType)) regs

fBo :: Functor f => String -> f [Register] -> f [Register]
fBo op = fmap (\x -> filterByOpType op x)

filterBetweenDates :: String -> [Register] -> [Register]
filterBetweenDates "" regs  = regs
filterBetweenDates dts regs = filter (\reg -> elem (date reg) range) regs
                                where
                                 wdts = words dts
                                 [dI, dF] = if length wdts == 2 then
                                              wdts
                                            else [dts, dts]
                                 range = [(read dI::Day)..(read dF::Day)]

fBd :: Functor f => String -> f [Register] -> f [Register]
fBd dts = fmap (\x -> filterBetweenDates dts x)

filterByTicker :: String -> [Register] -> [Register]
filterByTicker "all" regs = regs
filterByTicker tk    regs = filter (\reg -> elem (ticker reg) $ words tk) regs

fBt :: Functor f => String -> f [Register] -> f [Register]
fBt tk = fmap (\x -> filterByTicker tk x)

total :: [Register] -> (Double, Int)
total = foldr (\reg (v,q) -> 
                let amount = fromIntegral (quantity reg) * value reg
                in if opType reg == COMPRA then
                     (v + amount, q + (quantity reg))
                   else
                     (v - amount, q - (quantity reg)))
              (0,0)

fTotal :: Functor f => f [Register] -> f (Double, Int)
fTotal = fmap total

avg :: [Register] -> (Double, Int)
avg = foldr (\reg (pm,q) -> let fIq  = fromIntegral q
                                vReg = value reg
                                qReg = quantity reg
                                fqReg = fromIntegral qReg
                            in 
                              if opType reg == COMPRA then
                                (
                                  ((pm * fIq)
                                  + (vReg * fqReg)) 
                                  / fromIntegral (q + qReg)
                                , 
                                  q + qReg
                                )
                              else
                                (pm, q - qReg)
            )
            (0,0)

fAvg :: Functor f => f [Register] -> f (Double, Int)
fAvg = fmap avg

someFunc :: IO ()
someFunc = putStrLn "someFunc"
