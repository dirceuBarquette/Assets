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
   , hasValidTicker
   , hasValidCost
   , hasValidQuantity
   , hasValidDate
   , hasValidOpType
   , saveRegs2File
   , filteredFilesInCurdir
   , fromFiles2String
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

loadMain :: Int -> IO ()
loadMain option = do
   showScreen option
   if option == 0 then
      do newline
   else
      do 
         option <- getOption
         if option >= 0 then
            do newline
               case option of
                  0 -> loadMain 0
                  1 -> loadImport 1
                  2 -> loadFilter 2
                  3 -> loadSummarize 2
                  otherwise -> loadMain 9
         else
            do newline
               putStrLn "Opção inválida"
               loadMain 9

loadSummarize :: Int -> IO () -- mov 1 opção selecionar arquivo
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
                  otherwise -> loadMain 9
         else
            do newline
               putStrLn "Opção inválida"
               loadMain 9

loadSummarize2 :: Int -> IO () -- mov 2 lista arquivos para selecionar
loadSummarize2 option = do
   fileList <- filteredFilesInCurdir (\f -> isSuffixOf ".flt" f)
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
                  otherwise -> if null fileList then 
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

loadFilter :: Int -> IO () -- mov 1 opção selecionar arquivo
loadFilter option = do
   showScreen 2
   if option == 0 then
      do newline
   else
      do 
         option <- getOption
         if option >= 0 then
            do newline
               case option of
                  1 -> loadFilter2 2
                  otherwise -> loadMain 9
         else
            do newline
               putStrLn "Opção inválida"
               loadMain 9

loadFilter2 :: Int -> IO () -- mov 2 lista arquivos para selecionar
loadFilter2 option = do
   fileList <- filteredFilesInCurdir (\f -> isSuffixOf ".flt" f)
   putStr $ unlines $ zipWith (\n f -> show n ++ " - " ++ f) [1..] fileList
   showScreen 21
   if option == 0 then
      do newline
   else
      do 
         option <- getOption
         if option >= 0 then
            do newline
               case option of
                  0 -> loadFilter 9
                  otherwise -> if null fileList then 
                                 loadFilter 9
                               else file2filterLoaded 9 (fileList !! (option-1))
         else
            do newline
               putStrLn "Opção inválida"
               loadFilter2 9

file2filterLoaded :: Int -> String -> IO () -- mov 3 arquivo selecionado
file2filterLoaded option file = do          -- lista as opções de filtragem
   putStrLn $ "Arquivo carregado: " ++ file
   showScreen 22
   if option == 0 then
      do newline
   else
      do 
         option <- getOption
         if option >= 0 then
            do newline
               case option of
                  1 -> loadFilterByTicker 2 file
                  2 -> loadFilterByDate 2 file
                  3 -> loadFilterByOper 2 file
                  otherwise -> loadFilter2 9
         else
            do newline
               putStrLn "Opção inválida"
               loadMain 9

loadFilterByOper :: Int -> String -> IO () -- mov 4 
loadFilterByOper option file = do
   putStrLn $ "Arquivo carregado: " ++ file
   showScreen 25
   if option == 0 then
      do newline
   else
      do 
         oper <- fixLn
         newline
         runFilterByOper oper file

runFilterByOper :: String -> String -> IO ()
runFilterByOper oper path = do
   let rec = getRecords path
   let filtered = fBo oper rec
   let r2l = r2L filtered
   now <- getZonedTime
   let filtered = makePath ("filtered-",".flt") now
   saveRegs2File filtered r2l
   putStrLn $ "o arquivo " ++ filtered ++ " foi salvo"
   loadFilter2 9

loadFilterByDate :: Int -> String -> IO () -- mov 4 
loadFilterByDate option file = do
   putStrLn $ "Arquivo carregado: " ++ file
   showScreen 24
   if option == 0 then
      do newline
   else
      do 
         dates <- fixLn
         newline
         runFilterByDate dates file

runFilterByDate :: String -> String -> IO ()
runFilterByDate dates path = do
   let rec = getRecords path
   let filtered = fBd dates rec
   let r2l = r2L filtered
   now <- getZonedTime
   let filtered = makePath ("filtered-",".flt") now
   saveRegs2File filtered r2l
   putStrLn $ "o arquivo " ++ filtered ++ " foi salvo"
   loadFilter2 9

loadFilterByTicker :: Int -> String -> IO () -- mov 4 
loadFilterByTicker option file = do
   putStrLn $ "Arquivo carregado: " ++ file
   showScreen 23
   if option == 0 then
      do newline
   else
      do 
         tickers <- fixLn
         newline
         runFilterByTicker tickers file

runFilterByTicker :: String -> String -> IO ()
runFilterByTicker tickers path = do
   let rec = getRecords path
   let filtered = fBt tickers rec
   let r2l = r2L filtered
   now <- getZonedTime
   let filtered = makePath ("filtered-",".flt") now
   saveRegs2File filtered r2l
   putStrLn $ "o arquivo " ++ filtered ++ " foi salvo"
   loadFilter2 9

loadImport :: Int -> IO ()
loadImport option = do
   showScreen 1
   if option == 0 then
      do newline
   else
      do 
         option <- getOption
         if option >= 0 then
            do newline
               case option of
                  1 -> runImport
                  otherwise -> loadMain 9
         else
            do newline
               putStrLn "Opção inválida"
               loadMain 9

runImport :: IO ()
runImport = do 
   showScreen 11
   fileName <- fixLn
   let rec = getRecords fileName
   let r2l = r2L rec
   now <- getZonedTime
   let imported = makePath ("imported-",".flt") now
   saveRegs2File imported r2l
   putStrLn $ "o arquivo " ++ imported ++ " está disponível para filtragem"
   loadMain 9

makePath :: (String,String) -> ZonedTime -> String
makePath (prefix,suffix) zT = 
   formatTime defaultTimeLocale format $ zT
      where format = prefix ++ "%Y-%m-%d.%H%M%S" ++ suffix

newline :: IO ()
newline = putChar '\n'

fixLn :: IO String
fixLn = do
   ln <- getLine
   if fSpaces ln then fixLn else return ln
      where fSpaces str = and $ map isSpace str

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

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          loadMain 9
