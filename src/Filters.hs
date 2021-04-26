module Filters
   ( avg
   , total
   , filterByTicker
   , filterBetweenDates
   , filterByOpType
   ) where

import DataTypes (Register (..), OpType (..), Ticker, Summary)
import Data.Time (Day)

filterByOpType :: String -> [Register] -> [Register]
filterByOpType "" regs = regs
filterByOpType op regs = filter (\reg -> opType reg == (read op::OpType)) regs

filterBetweenDates :: String -> [Register] -> [Register]
filterBetweenDates "" regs  = regs
filterBetweenDates dts regs = filter (\reg -> elem (date reg) range) regs
                                where
                                 wdts = words dts
                                 [dI, dF] = if length wdts == 2 then
                                              wdts
                                            else [dts, dts]
                                 range = [(read dI::Day)..(read dF::Day)]

filterByTicker :: String -> [Register] -> [Register]
filterByTicker "all" regs = regs
filterByTicker tk    regs = filter (\reg -> elem (ticker reg) $ words tk) regs

cv :: OpType -> (Double, Int) -> (Double, Int) -> (Double, Int)
cv op (pm,q) (v,qop) = (pmf, qf)
      where
         qf  = if op == COMPRA then
                  sum [q, qop]
               else subtract qop q
         pmf = 
            case op of
               COMPRA -> (sum [pm * (fromIntegral q), v * (fromIntegral qop)])
                           / fromIntegral qf
               VENDA  -> if not (qf == 0) then pm else 0
         
avg :: [Register] -> (Double, Int)
avg = foldl (\ (pm,q) reg-> 
               cv (opType reg) (pm, q) (value reg, quantity reg)
             ) (0, 0)

total :: [Register] -> (Double, Int)
total = foldl (\(v,q) reg -> 
                let amount = fromIntegral (quantity reg) * value reg
                in if opType reg == COMPRA then
                     (v + amount, q + (quantity reg))
                   else
                     (v - amount, q - (quantity reg)))
              (0,0)
