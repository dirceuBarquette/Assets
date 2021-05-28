{-|                                                                                
Module      : Assets-tui 
Description : Text Interface functions
Copyright   : 2021 Dirceu Barquette
License     : BSD3
Maintainer  : dirceu.barquette@gmail.com

-}
module ATui where

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
   , hasValidTicker
   , hasValidCost
   , hasValidQuantity
   , hasValidDate
   , hasValidOpType
   , saveRegs2File
   , filteredFilesInCurdir
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
   , fSummarize
   )
import CommandLine
   ( showScreen
   )
import System.IO                                                                   
import Data.Char
   ( isDigit
   , isSpace
   , digitToInt
   , toUpper
   )
import Data.List
   ( isSuffixOf
   )
import Data.Time
   ( getZonedTime
   , formatTime
   , defaultTimeLocale
   , ZonedTime
   )
import System.Console.ANSI 
   ( SGR(..)
   , ConsoleIntensity(..)
   , setSGR
   )

loadMain :: Int -> IO ()
loadMain option = do
   if option == 0 then
      putStrLn "Até a próxima!"
   else
      do 
         newline
         showScreen option
         option <- getOption
         if option >= 0 then
            do newline
               case option of
                  0 -> loadMain 0
                  1 -> loadImport
                  2 -> loadFilter
                  3 -> loadSummarize 2
                  _ -> loadMain 9
         else
            do newline
               putStrLn "Opção inválida"
               loadMain 9

loadSummarize :: Int -> IO ()
loadSummarize option = do
   showScreen 3
   if option == 0 then
      do newline
   else
      do 
         option <- getOption
         if option >= 0 then
            do newline
               case option of
                  1 -> loadSummarize2 2
                  _ -> loadMain 9
         else
            do newline
               putStrLn "Opção inválida"
               loadMain 9

loadSummarize2 :: Int -> IO ()
loadSummarize2 option = do
   fileList <- filteredFilesInCurdir (isSuffixOf ".flt")
   putStr $ unlines $ zipWith (\n f -> show n ++ " - " ++ f) [1..] fileList
   showScreen 26
   if option == 0 then
      do newline
   else
      do 
         option <- getOption
         if option >= 0 then
            do newline
               case option of
                  0 -> loadSummarize 9
                  _ -> if null fileList || option > length fileList then 
                          loadSummarize 9
                       else runSummarize (fileList !! (option-1))
         else
            do newline
               putStrLn "Opção inválida"
               loadSummarize2 9

runSummarize :: String -> IO ()
runSummarize path = do
   let rec = getRecords path
   let fsumm = fSummarize rec
   let s2l = s2L fsumm
   now <- getZonedTime
   let summarized = makePath ("summary-",".txt") now
   saveRegs2File summarized s2l
   putStrLn $ "o arquivo " ++ summarized ++ " foi salvo"
   loadSummarize2 9

loadFilter :: IO ()
loadFilter = do
   showScreen 2
   option <- getOption
   if option >= 0 then
      do newline
         case option of
            1 -> loadFilter2
            _ -> loadMain 9
   else
      do newline
         putStrLn "Opção inválida"
         loadMain 9

loadFilter2 :: IO ()
loadFilter2 = do
   fileList <- filteredFilesInCurdir (isSuffixOf ".flt")
   putStr $ unlines $ zipWith (\n f -> show n ++ " - " ++ f) [1..] fileList
   showScreen 21
   option <- getOption
   if option >= 0 then
      do newline
         case option of
            0 -> loadFilter
            9 -> loadFilter
            _ -> if null fileList || option > length fileList then 
                    loadFilter
                 else file2filterLoaded (fileList !! (option-1))
   else
      do newline
         putStrLn "Opção inválida"
         loadFilter2

file2filterLoaded :: String -> IO ()
file2filterLoaded file = do
   putStrLn $ "Arquivo carregado: " ++ file
   showScreen 22
   option <- getOption
   if option >= 0 then
      do newline
         case option of
            1 -> loadFilterByTicker file
            2 -> loadFilterByDate file
            3 -> loadFilterByOper file
            _ -> loadFilter2
   else
      do newline
         putStrLn "Opção inválida"
         loadMain 9

loadFilterByOper :: String -> IO ()
loadFilterByOper file = do
   putStrLn $ "Arquivo carregado: " ++ file
   showScreen 25
   oper <- fixLn
   let oper2upper = map toUpper oper
   newline
   if hasValidOpType oper2upper then
      runFilterByOper oper2upper file
   else do
      putStrLn "tipo de ordem inválida!"
      newline
      file2filterLoaded file

runFilterByOper :: String -> String -> IO ()
runFilterByOper oper path = do
   let rec = getRecords path
   let filtered = fBo oper rec
   saveFiltered filtered

loadFilterByDate :: String -> IO ()
loadFilterByDate file = do
   putStrLn $ "Arquivo carregado: " ++ file
   showScreen 24
   dates <- fixLn
   let parseDates = map hasValidDate $ words dates
   newline
   if and parseDates then
      runFilterByDate dates file
   else do
      putStrLn "algo está errado na data!"
      newline
      file2filterLoaded file

runFilterByDate :: String -> String -> IO ()
runFilterByDate dates path = do
   let rec = getRecords path
   let filtered = fBd dates rec
   saveFiltered filtered

loadFilterByTicker :: String -> IO ()
loadFilterByTicker file = do
   putStrLn $ "Arquivo carregado: " ++ file
   showScreen 23
   tickers <- fixLn
   newline
   runFilterByTicker tickers file

runFilterByTicker :: String -> String -> IO ()
runFilterByTicker tickers path = do
   let rec = getRecords path
   let filtered = fBt tickers rec
   saveFiltered filtered

loadImport :: IO ()
loadImport = do
   showScreen 1
   option <- getOption
   if option >= 0 then
      do newline
         case option of
            1 -> runImport
            _ -> loadMain 9
   else
      do newline
         putStrLn "Opção inválida"
         loadMain 9

runImport :: IO ()
runImport = do 
   showScreen 11
   fileName <- fixLn
   let rec = getRecords fileName
   isEmpty <- null <$> rec
   if isEmpty then do
      putStrLn $ "Não foi possível ler registros de " ++ fileName
   else do 
      let r2l = r2L rec
      now <- getZonedTime
      let imported = makePath ("imported-",".flt") now
      saveRegs2File imported r2l
      putStrLn $ "o arquivo " ++ imported ++ " está disponível para filtragem"
   loadMain 9

makePath :: (String,String) -> ZonedTime -> String
makePath (prefix,suffix) = 
   formatTime defaultTimeLocale format
      where format = prefix ++ "%Y-%m-%d.%H%M%S" ++ suffix

saveFiltered :: IO [Register] -> IO ()
saveFiltered filtered = do 
   let r2l = r2L filtered
   now <- getZonedTime
   let filtered = makePath ("filtered-",".flt") now
   saveRegs2File filtered r2l
   putStrLn $ "o arquivo " ++ filtered ++ " foi salvo"
   newline
   loadFilter2

newline :: IO ()
newline = putChar '\n'

fixLn :: IO String
fixLn = do
   ln <- getLine
   if fSpaces ln then fixLn else return ln
      where fSpaces str = all isSpace str

getNonEmptyChar :: IO Char
getNonEmptyChar = do
   v <- getChar
   if isSpace v then getNonEmptyChar else return v

getOption :: IO Int
getOption = do 
   x <- getNonEmptyChar
   if isDigit x  then
      return (digitToInt x)
   else
      do newline
         putStrLn "Dígito inválido!"
         return 9

--main :: IO ()
--main = do hSetBuffering stdout NoBuffering
--          loadMain 9
