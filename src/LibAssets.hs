{-|                                                                                
Module      : LibAssets
Description : The basic API for getting entries and filtering them
Copyright   : 2021 Dirceu Barquette
License     : BSD3
Maintainer  : dirceu.barquette@gmail.com

-}
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
   , hasValidTicker
   , hasValidCost
   , hasValidQuantity
   , hasValidDate
   , hasValidOpType
   , filteredFilesInCurdir
   ) where

import Filters
   ( avg
   , total
   , filterByTicker
   , filterBetweenDates
   , filterByOpType
   )
import DataTypes
   ( Register (..)
   , OpType (..)
   , Ticker
   , Summary
   )
import Reports
   ( reg2Lists
   , summ2Lists
   )
import System.IO ()
import System.Directory
   ( getCurrentDirectory
   , getDirectoryContents
   )
import Data.Time
   ( Day )
import Data.List.Split
   ( splitOn )
import Data.Char
   ( digitToInt
   , isDigit
   , isAlphaNum
   )
import Control.Exception.Base
   ( try
   , SomeException
   )

getRecordFrom :: FilePath -> IO (Either SomeException String)
getRecordFrom file2read = do try (readFile file2read)

validating :: [[String]] -> [[String]]
validating = filter (\y -> combo (y !! 1) (head y)
                                 (y !! 3) (y !! 4) (y !! 2))
   where combo op dt qt cs tk = 
           hasValidOpType op
           && hasValidDate dt
           && hasValidQuantity qt
           && hasValidCost cs
           && hasValidTicker tk

-- | Gets entries from a valid format file 
getRecords :: FilePath -> IO [Register]
getRecords path = do
   contents <- getRecordFrom path
   case contents of
      Left  _   -> return [] 
      Right str -> do
         let lns = lines str
         let ws = map words lns
         let fBy5fields = filter ((== 5) . length) ws
         let validEntries = validating fBy5fields
         let regs = map (\x -> Register (x !! 2) 
                                        (read (head x) :: Day) 
                                        (read (x !! 1) :: OpType)
                                        (read (x !! 3) :: Int)
                                        (read (x !! 4) :: Double)) validEntries
         return regs

-- | This function parses a 'ticker' field of the 'Register' type
hasValidTicker :: String -> Bool
hasValidTicker str = (length str < 10) && isalpha str && notNull str
   where isalpha s = all isAlphaNum str
         notNull s = not $ null str

-- | This function parses a 'value' field of the 'Register' type
hasValidCost :: String -> Bool
hasValidCost str = all isDigit (concat $ splitOn "." str)
                     && not (null str)

-- | This function parses a 'quantity' field of the 'Register' type
hasValidQuantity :: String -> Bool
hasValidQuantity str = all isDigit str && not (null str)

-- | This function parses a 'date' field of the 'Register' type
hasValidDate :: String -> Bool
hasValidDate str = and [ has10chars str
                       , has3fields str
                       , ymdLengthOK
                       , toDate
                       ]
   where has10chars  s = length s == 10
         splitedDate s = splitOn "-" s
         [y,m,d]       = splitOn "-" str
         has3fields  s = length (splitedDate s) == 3
         ymdLengthOK   = (length y == 4) && (length m == 2) && (length d == 2)
         allAreDigits  = all isDigit (concat [y,m,d])
         toDate        = allAreDigits
                           &&
                             (let toYear  = sum $ zipWith (*) [1000,100,10,1]
                                               $ map digitToInt y
                                  toMonth = sum $ zipWith (*) [10,1]
                                               $ map digitToInt m 
                                  toDay   = sum $ zipWith (*) [10,1]
                                               $ map digitToInt d 
                             in and [ toYear  > 1899
                                    , toYear  < 2201
                                    , toMonth > 0
                                    , toMonth < 13
                                    , toDay   > 0
                                    , toDay   < 32
                                    ])

-- | This function parses a 'opType' field of the 'Register' type
hasValidOpType :: String -> Bool
hasValidOpType str | str `elem` oPs = True
                   | otherwise      = False
                      where oPs = ["COMPRA", "VENDA"]

-- | This function gets a directory content. It fetches by \".txt\" or
-- \".flt\" file types
filteredFilesInCurdir :: (FilePath -> Bool) -> IO [FilePath]
filteredFilesInCurdir f = filter f <$> 
                           (getCurrentDirectory >>= getDirectoryContents)

-- | This function converts from a 'Summary' type to be saved
s2L :: Functor f => f Summary -> f [String]
s2L = fmap summ2Lists

-- | This function converts from a 'Register' type to be saved
r2L :: Functor f => f [Register] -> f [String]
r2L = fmap reg2Lists

-- | This function saves a file of filtered entries
saveRegs2File :: FilePath -> IO [String] -> IO ()
saveRegs2File path r2l = r2l >>= writeFile path . unlines

-- | This function operates as the main 'opType' filtering command
fBo :: Functor f => String -> f [Register] -> f [Register]
fBo op = fmap (filterByOpType op)

-- | This function operates as the main 'date' filtering command
fBd :: Functor f => String -> f [Register] -> f [Register]
fBd dts = fmap (filterBetweenDates dts)

-- | This function operates as the main 'ticker' filtering command
fBt :: Functor f => String -> f [Register] -> f [Register]
fBt tk = fmap (filterByTicker tk)

-- | This function sums the total cost of a 'Register' list
fTotal :: Functor f => f [Register] -> f (Double, Int)
fTotal = fmap total

-- | This function gets the average cost of a 'Register' list
fAvg :: Functor f => f [Register] -> f (Double, Int)
fAvg = fmap avg
