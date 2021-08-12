{-|                                                                                
Module      : Assets-tui 
Description : Text Interface functions
Copyright   : 2021 Dirceu Barquette
License     : BSD3
Maintainer  : dirceu.barquette@gmail.com

-}
module ATui where

import DataTypes
   ( Register )
import LibAssets
   ( getRecords
   , fBt
   , fBd
   , fBo
   , r2L
   , s2L
   , hasValidDate
   , hasValidOpType
   , saveRegs2File
   , filteredFilesInCurdir
   )
import Reports
   ( fSummarize )
import CommandLine
   ( showScreen )
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

loadMain :: String -> Int -> IO ()
loadMain lang option = do
   if option == 0 then
      putStrLn $ if lang == "" then "Até a próxima!" else "See you next time!"
   else
      do 
         newline
         showScreen lang option
         option <- getOption lang
         if option >= 0 then
            do newline
               case option of
                  0 -> loadMain lang 0
                  1 -> loadImport lang
                  2 -> loadFilter lang
                  3 -> loadSummarize lang 2
                  _ -> loadMain lang 9
         else
            do newline
               invalidOptStr lang
               loadMain lang 9

loadSummarize :: String -> Int -> IO ()
loadSummarize lang option = do
   showScreen lang 3
   if option == 0 then
      do newline
   else
      do 
         option <- getOption lang
         if option >= 0 then
            do newline
               case option of
                  1 -> loadSummarize2 lang 2
                  _ -> loadMain lang 9
         else
            do newline
               invalidOptStr lang
               loadMain lang 9

loadSummarize2 :: String -> Int -> IO ()
loadSummarize2 lang option = do
   fileList <- filteredFilesInCurdir (isSuffixOf ".flt")
   putStr $ unlines $ zipWith (\n f -> show n ++ " - " ++ f) [1..] fileList
   showScreen lang 26
   if option == 0 then
      do newline
   else
      do 
         option <- getOption lang
         if option >= 0 then
            do newline
               case option of
                  0 -> loadSummarize lang 9
                  _ -> if null fileList || option > length fileList then 
                          loadSummarize lang 9
                       else runSummarize lang (fileList !! (option-1))
         else
            do newline
               invalidOptStr lang
               loadSummarize2 lang 9

runSummarize :: String -> String -> IO ()
runSummarize lang path = do
   let rec = getRecords path
   let fsumm = fSummarize rec
   let s2l = s2L fsumm
   now <- getZonedTime
   let summarized = makePath ("summary-",".txt") now
   saveRegs2File summarized s2l
   let (thefile,issaved) = if null lang then ("o arquivo "," foi salvo!")
                           else ("the file ", " is saved!")
   putStrLn $ thefile ++ summarized ++ issaved
   loadSummarize2 lang 9

loadFilter :: String -> IO ()
loadFilter lang = do
   showScreen lang 2
   option <- getOption lang
   if option >= 0 then
      do newline
         case option of
            1 -> loadFilter2 lang
            _ -> loadMain lang 9
   else
      do newline
         invalidOptStr lang
         loadMain lang 9

loadFilter2 :: String -> IO ()
loadFilter2 lang = do
   fileList <- filteredFilesInCurdir (isSuffixOf ".flt")
   putStr $ unlines $ zipWith (\n f -> show n ++ " - " ++ f) [1..] fileList
   showScreen lang 21
   option <- getOption lang
   if option >= 0 then
      do newline
         case option of
            0 -> loadFilter lang
            9 -> loadFilter lang
            _ -> if null fileList || option > length fileList then 
                    loadFilter lang
                 else file2filterLoaded lang (fileList !! (option-1))
   else
      do newline
         invalidOptStr lang
         loadFilter2 lang

file2filterLoaded :: String -> String -> IO ()
file2filterLoaded lang file = do
   let msg = if null lang then "arquivo carregado: " else "loaded file: "
   putStrLn $ msg ++ file
   showScreen lang 22
   option <- getOption lang
   if option >= 0 then
      do newline
         case option of
            1 -> loadFilterByTicker lang file
            2 -> loadFilterByDate lang file
            3 -> loadFilterByOper lang file
            _ -> loadFilter2 lang
   else
      do newline
         invalidOptStr lang
         loadMain lang 9

loadFilterByOper :: String -> String -> IO ()
loadFilterByOper lang file = do
   let msg = if null lang then "arquivo carregado: " else "loaded file: "
   putStrLn $ msg ++ file
   showScreen lang 25
   oper <- fixLn
   let oper2upper = map toUpper oper
   newline
   if hasValidOpType oper2upper then
      runFilterByOper lang oper2upper file
   else do
      putStrLn $ if null lang then "tipo de ordem inválida!"
                  else "Invalid order type!"
      newline
      file2filterLoaded lang file

runFilterByOper :: String -> String -> String -> IO ()
runFilterByOper lang oper path = do
   let rec = getRecords path
   let filtered = fBo oper rec
   saveFiltered lang filtered

loadFilterByDate :: String -> String -> IO ()
loadFilterByDate lang file = do
   let msg = if null lang then "Arquivo carregado: " else "loaded file: "
   putStrLn $ msg ++ file
   showScreen lang 24
   dates <- fixLn
   let parseDates = map hasValidDate $ words dates
   newline
   if and parseDates then
      runFilterByDate lang dates file
   else do
      let err = if null lang then "algo está errado na data!"
                else "Incorrect date format!"
      putStrLn err
      newline
      file2filterLoaded lang file

runFilterByDate :: String -> String -> String -> IO ()
runFilterByDate lang dates path = do
   let rec = getRecords path
   let filtered = fBd dates rec
   saveFiltered lang filtered

loadFilterByTicker :: String -> String -> IO ()
loadFilterByTicker lang file = do
   let msg = if null lang then "Arquivo carregado: " else "loaded file: "
   putStrLn $ msg ++ file
   showScreen lang 23
   tickers <- fixLn
   newline
   runFilterByTicker lang tickers file

runFilterByTicker :: String -> String -> String -> IO ()
runFilterByTicker lang tickers path = do
   let rec = getRecords path
   let filtered = fBt tickers rec
   saveFiltered lang filtered

loadImport :: String -> IO ()
loadImport lang = do
   showScreen lang 1
   option <- getOption lang
   if option >= 0 then
      do newline
         case option of
            1 -> runImport lang
            _ -> loadMain lang 9
   else
      do newline
         invalidOptStr lang
         loadMain lang 9

runImport :: String -> IO ()
runImport lang = do 
   showScreen lang 11
   fileName <- fixLn
   let rec = getRecords fileName
   isEmpty <- null <$> rec
   if isEmpty then do
      let msg = if null lang then "Não foi possível ler registros de "
                else "can't read registers from "
      putStrLn $ msg ++ fileName
   else do 
      let r2l = r2L rec
      now <- getZonedTime
      let imported = makePath ("imported-",".flt") now
      saveRegs2File imported r2l
      let (thefile,isavailable) = if null lang
                                   then ("o arquivo "," está disponível para filtragem")
                                  else ("The file "," is available for filtering") 
      putStrLn $ thefile ++ imported ++ isavailable
   loadMain lang 9

makePath :: (String,String) -> ZonedTime -> String
makePath (prefix,suffix) = 
   formatTime defaultTimeLocale format
      where format = prefix ++ "%Y-%m-%d.%H%M%S" ++ suffix

saveFiltered :: String -> IO [Register] -> IO ()
saveFiltered lang filtered = do 
   let r2l = r2L filtered
   now <- getZonedTime
   let filtered = makePath ("filtered-",".flt") now
   saveRegs2File filtered r2l
   let (thefile,issaved) = if null lang then ("o arquivo "," foi salvo")
                            else ("The file "," is saved") 
   putStrLn $ thefile ++ filtered ++ issaved
   newline
   loadFilter2 lang

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

getOption :: String -> IO Int
getOption lang = do 
   x <- getNonEmptyChar
   if isDigit x  then
      return (digitToInt x)
   else
      do newline
         putStrLn $ if null lang then "Dígito inválido!" else "Invalid digit!"
         return 9

invalidOptStr :: String -> IO ()
invalidOptStr lang | lang == "en" = putStrLn "Invalid option"
                   | otherwise    = putStrLn "Opção inválida"

