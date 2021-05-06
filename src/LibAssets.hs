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

hasValidTicker :: String -> Bool
hasValidTicker str = (length str < 10) && isalpha str && notNull str
   where isalpha s = all isAlphaNum str
         notNull s = not $ null str

hasValidCost :: String -> Bool
hasValidCost str = all isDigit (concat $ splitOn "." str)
                     && not (null str)

hasValidQuantity :: String -> Bool
hasValidQuantity str = all isDigit str && not (null str)

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

hasValidOpType :: String -> Bool
hasValidOpType str | str `elem` oPs = True
                   | otherwise      = False
                      where oPs = ["COMPRA", "VENDA"]

filteredFilesInCurdir :: (FilePath -> Bool) -> IO [FilePath]
filteredFilesInCurdir f = filter f <$> 
                           (getCurrentDirectory >>= getDirectoryContents)

s2L :: Functor f => f Summary -> f [String]
s2L = fmap summ2Lists

r2L :: Functor f => f [Register] -> f [String]
r2L = fmap reg2Lists

saveRegs2File :: FilePath -> IO [String] -> IO ()
saveRegs2File path r2l = r2l >>= writeFile path . unlines

fBo :: Functor f => String -> f [Register] -> f [Register]
fBo op = fmap (filterByOpType op)

fBd :: Functor f => String -> f [Register] -> f [Register]
fBd dts = fmap (filterBetweenDates dts)

fBt :: Functor f => String -> f [Register] -> f [Register]
fBt tk = fmap (filterByTicker tk)

fTotal :: Functor f => f [Register] -> f (Double, Int)
fTotal = fmap total

fAvg :: Functor f => f [Register] -> f (Double, Int)
fAvg = fmap avg
