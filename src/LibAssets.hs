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
    , fromFiles2String
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
import System.Directory
   ( getCurrentDirectory
   , getDirectoryContents
   )
import Data.Time ( Day )
import Data.List.Split ( splitOn )
import Data.Char ( digitToInt
                 , isDigit
                 , isAlphaNum
                 )

validating :: [[String]] -> [[String]]
validating = filter (\y -> combo (y !! 1) (y !! 0)
                                 (y !! 3) (y !! 4) (y !! 2))
   where combo op dt qt cs tk = 
           ( hasValidOpType op
           && hasValidDate dt
           && hasValidQuantity qt
           && hasValidCost cs
           && hasValidTicker tk
           )

getRecords :: FilePath -> IO [Register]
getRecords path = do
   contents <- readFile path
   let lns = lines contents
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
hasValidTicker str = and [length str < 10, isalpha str, notNull str]
   where isalpha s = and $ (map isAlphaNum str)
         notNull s = not $ null str

hasValidCost :: String -> Bool
hasValidCost str = (and $ map isDigit $ concat $ splitOn "." str)
                     && (not $ null str)

hasValidQuantity :: String -> Bool
hasValidQuantity str = (and $ map isDigit str) && (not $ null str)

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
         ymdLengthOK   = and [length y == 4, length m == 2, length d == 2]
         allAreDigits  = and $ map isDigit $ concat [y,m,d]
         toDate        = if allAreDigits then
                           let toYear  = sum $ zipWith (*) [1000,100,10,1]
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
                                  ]
                         else False

hasValidOpType :: String -> Bool
hasValidOpType str | str `elem` oPs = True
                   | otherwise      = False
                      where oPs = ["COMPRA", "VENDA"]

-- thanks to https://stackoverflow.com/questions/6649347/
-- how-to-list-all-files-in-current-directory
filteredFilesInCurdir :: (FilePath -> Bool) -> IO [FilePath]
filteredFilesInCurdir f = getCurrentDirectory >>= getDirectoryContents
                           >>= return . filter f

fromFiles2String :: IO [FilePath] -> IO [String]
fromFiles2String paths = fmap (\p -> p) paths
   --where filteredFilesInCurdir (\str -> isSuffixOf suffix str)

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
