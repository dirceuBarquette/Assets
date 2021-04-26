module Main where

import DataTypes
   ( OpType
   , Register
   , Ticker
   , Summary
   )
import LibAssets
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
   )
import Filters
   ( avg
   , total
   , filterByTicker
   , filterBetweenDates
   , filterByOpType
   )
import Reports
   ( reg2Lists
   , summ2Lists
   )

main :: IO ()
main = someFunc
