{-|                                                                                
Module      : Filters
Description : data structures
Copyright   : 2021 Dirceu Barquette
License     : BSD3
Maintainer  : dirceu.barquette@gmail.com

-}

module Filters
   ( avg
   , total
   , filterByTicker
   , filterBetweenDates
   , filterByOpType
   ) where

import DataTypes
   ( Register(..)
   , OpType(..)
   , Ticker
   , Summary
   )
import Data.Time
   ( Day )

-- | This function filters the entries by 'OpType' data type: 'COMPRA' or
-- 'VENDA'
filterByOpType :: String -> [Register] -> [Register]
filterByOpType "" regs = regs
filterByOpType op regs = filter (\reg -> opType reg == (read op::OpType)) regs

-- | This function filters the entries by date using yyyy-mm-dd. The date
-- format is provided by the Data module
filterBetweenDates :: String -- ^ accepts single date (yyyy-mm-dd)or between
                             -- dates (yyyy-mm-dd yyyy-mm-dd)
                   -> [Register] -> [Register]
filterBetweenDates "" regs  = regs
filterBetweenDates dts regs = filter (\reg -> date reg `elem` range) regs
                                where
                                 wdts = words dts
                                 [dI, dF] = if length wdts == 2 then
                                              wdts
                                            else [dts, dts]
                                 range = [(read dI::Day)..(read dF::Day)]

-- | This function filters the entries by 'Ticker' 
filterByTicker :: String -- ^ accepts one or more tickers separated by blanks
               -> [Register] -> [Register]
filterByTicker "all" regs = regs
filterByTicker tk    regs = filter (\reg -> elem (ticker reg) $ words tk) regs

-- | This function is an auxiliary function for the avg function 
cv :: OpType -> (Double, Int) -> (Double, Int) -> (Double, Int)
cv op (pm,q) (v,qop) = (pmf, qf)
      where
         qf  = if op == COMPRA then
                  sum [q, qop]
               else subtract qop q
         pmf = 
            case op of
               COMPRA -> sum [pm * fromIntegral q, v * fromIntegral qop]
                           / fromIntegral qf
               VENDA  -> if qf /= 0 then pm else 0

-- | This function calculates the average cost of the records provided
avg :: [Register] -> (Double, Int)
avg = foldl (\ (pm,q) reg-> 
               cv (opType reg) (pm, q) (value reg, quantity reg)
             ) (0, 0)

-- | This function calculates the total value of the records provided
total :: [Register] -> (Double, Int)
total = foldl (\(v,q) reg -> 
                let amount = fromIntegral (quantity reg) * value reg
                in if opType reg == COMPRA then
                     (v + amount, q + quantity reg)
                   else
                     (v - amount, q - quantity reg))
              (0,0)
