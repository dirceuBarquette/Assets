{-|                                                                                
Module      : Reports
Description : data structures
Copyright   : 2021 Dirceu Barquette
License     : BSD3
Maintainer  : dirceu.barquette@gmail.com

-}
module Reports
   ( reg2Lists
   , summ2Lists
   , fSummarize
   ) where

import DataTypes
   (Register (..)
   , OpType (..)
   , Ticker
   , Summary
   )
import Filters
   ( filterByTicker
   , total
   , avg
   )
import Data.List
   ( concatMap
   , nub
   )

reg2Lists :: [Register] -> [String]
reg2Lists = map (\reg -> unwords
                  [ show (date reg)
                  , show (opType reg)
                  , ticker reg
                  , show (quantity reg)
                  , show (value reg)
                  ])

getTickers :: [Register] -> [Ticker]
getTickers rl = 
   let tks = fmap ticker rl
   in nub tks

summ2Lists :: Summary -> [String]
summ2Lists = fmap (\(tk, (tot, qtt), avg, regs) -> 
                      tk ++ " "
                      ++ show qtt ++ " "
                      ++ show (truncate' tot 2) ++ " "
                      ++ show (truncate' avg 2) ++ "\n"
                      ++ (replicate 80 '=' :: String) ++ "\n"
                      ++ concatMap
                           (\reg ->
                              show (date reg) ++ " "
                              ++ show (opType reg) ++ " "
                              ++ show (quantity reg) ++ " "
                              ++ show (value reg) ++ "\n"
                         ) regs)

summarize :: [Register] -> Summary
summarize rec =
   let tks = getTickers rec
   in fmap (\tk -> let fbt = filterByTicker tk rec
                   in (tk, total fbt, fst $ avg fbt, fbt)) tks 

fSummarize :: Functor f => f [Register] -> f Summary
fSummarize = fmap summarize

-- thx to
-- stackoverflow.com/questions/18723381/
-- rounding-to-specific-number-of-digits-in-haskell
truncate' :: Double -> Int -> Double
truncate' x n = fromIntegral (floor (x * t)) / t
    where t = 10^n

