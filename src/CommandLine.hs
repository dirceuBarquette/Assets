{-|                                                                                
Module      : CommandLine
Description : This module is basically a displays library
Copyright   : 2021 Dirceu Barquette
License     : BSD3
Maintainer  : dirceu.barquette@gmail.com

Provides the menu items displayed while the user interacts with application on the command line
-}
module CommandLine
   ( showScreen
   ) where

import System.Console.ANSI 
   ( SGR
      ( Reset
      , SetColor  
      , SetConsoleIntensity
      )
   , ConsoleIntensity( BoldIntensity )
   , setSGR
   , ConsoleLayer
      ( Foreground
      )
   , Color
      ( White
      , Green
      )
   , ColorIntensity
      ( Vivid
      , Dull
      )
   )

type Lang     = String
type Menu     = [String]
type Prompt   = String
type Screen   = (Int, (Menu, Prompt))

-- | This function aggregates and shows the menu items available
-- to interacts with the user
showScreen :: Lang -> Int -> IO ()
showScreen lang i = do
   setSGR [ SetConsoleIntensity BoldIntensity
          , SetColor Foreground Dull White
          ]
   putStr theMenu
   putStr thePrompt 
   setSGR [Reset]
   setSGR [ 
            SetColor Foreground Vivid Green
          , SetConsoleIntensity BoldIntensity
          ]
   putStr " > "
   setSGR [Reset]
      where (Just (menu, prompt)) = lookup i (screens lang)
            theMenu   = unlines menu
            thePrompt = prompt

screens :: Lang -> [Screen]
screens lang = [ exitScreen lang
               , mainScreen lang
               , importScreen lang
               , importScreen2 lang
               , filterScreen lang
               , filterScreen2 lang
               , file2BeFilteredScreen lang
               , filterByTickerScreen lang
               , filterByDateScreen lang
               , filterByOperScreen lang
               , summarizeScreen lang
               , summarizeScreen2 lang
               ]

-- begin screens
exitScreen :: Lang -> Screen
exitScreen lang = (0, (exitMenu lang, exitPrompt lang))

filterScreen :: Lang -> Screen
filterScreen lang = (2, (filterMenu lang, filterPrompt lang))

summarizeScreen :: Lang -> Screen
summarizeScreen lang = (3, (summarizeMenu lang, summarizePrompt lang))

filterScreen2 :: Lang -> Screen
filterScreen2 lang = (21, (filterMenu2 lang, filterPrompt2 lang))

summarizeScreen2 :: Lang -> Screen
summarizeScreen2 lang = (26, (summarizeMenu2 lang, summarizePrompt2 lang))

filterByTickerScreen :: Lang -> Screen
filterByTickerScreen lang = (23, (filterByTickerMenu lang, filterByTickerPrompt lang))

filterByDateScreen :: Lang -> Screen
filterByDateScreen lang = (24, (filterByDateMenu lang, filterByDatePrompt lang))

filterByOperScreen :: Lang -> Screen
filterByOperScreen lang = (25, (filterByOperMenu lang, filterByOperPrompt lang))

file2BeFilteredScreen :: Lang -> Screen
file2BeFilteredScreen lang = (22, (file2BeFilteredMenu lang, file2BeFilteredPrompt lang))

mainScreen :: Lang -> Screen
mainScreen lang = (9, (mainMenu lang, mainPrompt lang))

importScreen :: Lang -> Screen
importScreen lang = (1, (importMenu lang, importPrompt lang))

importScreen2 :: Lang -> Screen
importScreen2 lang = (11, (importMenu2 lang, importPrompt2 lang))
-- end screens

-- begin menus
exitMenu :: Lang -> Menu
exitMenu "en" = []
exitMenu _    = []

filterMenu :: Lang -> Menu
filterMenu "en" = ["1 - Select file", "0 - Back"]
filterMenu _    = ["1 - Selecionar arquivo", "0 - Voltar"]

summarizeMenu :: Lang -> Menu
summarizeMenu "en" = ["1 - Select file", "0 - Back"]
summarizeMenu _    = ["1 - Selecionar arquivo", "0 - Voltar"]

summarizeMenu2 :: Lang -> Menu
summarizeMenu2 "en" = ["0 - Back"]
summarizeMenu2 _    = ["0 - Voltar"]

filterMenu2 :: Lang -> Menu
filterMenu2 "en" = ["0 - Back"]
filterMenu2 _    = ["0 - Voltar"]

filterByTickerMenu :: Lang -> Menu
filterByTickerMenu "en" = []
filterByTickerMenu _    = []

filterByDateMenu :: Lang -> Menu
filterByDateMenu "en" = []
filterByDateMenu _    = []

filterByOperMenu :: Lang -> Menu
filterByOperMenu "en" = []
filterByOperMenu _    = []

file2BeFilteredMenu :: Lang -> Menu
file2BeFilteredMenu "en" =
   [ "1 - Filter by ticker(s). i.e.: ABCD3, ABCD3 EFGH4 KLMN11"
   , "2 - Filter by date(s). i.e.: 2019-01-01 2019-12-31, 2020-04-01"
   , "3 - Filter by order type. i.e.: COMPRA, VENDA"
   , "0 - Back"
   ] 
file2BeFilteredMenu _    = 
   [ "1 - Filtrar por ticker(s). Ex.: ABCD3, ABCD3 EFGH4 KLMN11"
   , "2 - Filtrar por data(s). Ex.: 2019-01-01 2019-12-31, 2020-04-01"
   , "3 - Filtrar por tipo de ordem. Ex.: COMPRA, VENDA"
   , "0 - Voltar"
   ]

importMenu :: Lang -> Menu
importMenu "en" = ["1 - Import", "0 - Back"]
importMenu _    = ["1 - Importar", "0 - Voltar"]

importMenu2 :: Lang -> Menu
importMenu2 "en" = []
importMenu2 _    = []

mainMenu :: Lang -> Menu
mainMenu "en" = ["1 - Import", "2 - Filter", "3 - Sumarize","0 - Exit"]
mainMenu _    = ["1 - Importar", "2 - Filtrar", "3 - Sumarizar","0 - Sair"]
-- end menus

-- begin prompts
defaultPrompt :: Lang -> String
defaultPrompt "en" = "type an option"
defaultPrompt _    = "digite uma opção"

exitPrompt :: Lang -> Prompt
exitPrompt "en" = "See you next time"
exitPrompt _    = "Até a próxima!"

filterPrompt :: Lang -> Prompt
filterPrompt lang = defaultPrompt lang

summarizePrompt :: Lang -> Prompt
summarizePrompt lang = defaultPrompt lang

summarizePrompt2 :: Lang -> Prompt
summarizePrompt2 lang = defaultPrompt lang

filterPrompt2 :: Lang -> Prompt
filterPrompt2 lang = defaultPrompt lang

file2BeFilteredPrompt :: Lang -> Prompt
file2BeFilteredPrompt lang = defaultPrompt lang

filterByTickerPrompt :: Lang -> Prompt
filterByTickerPrompt "en" =
   "Filter by ticker(s). i.e.: ABCD3, ABCD3 EFGH4 KLMN11"
filterByTickerPrompt _    =
   "Filtrar por ticker(s). Ex.: ABCD3, ABCD3 EFGH4 KLMN11"

filterByDatePrompt :: Lang -> Prompt
filterByDatePrompt "en" =
   "Filter by date(s). i.e.: 2019-01-01 2019-12-31, 2020-04-01"
filterByDatePrompt _    =
   "Filtrar por data(s). Ex.: 2019-01-01 2019-12-31, 2020-04-01"

filterByOperPrompt :: Lang -> Prompt
filterByOperPrompt "en" =
   "Filter by order type. i.e.: COMPRA, VENDA"
filterByOperPrompt _    =
   "Filtrar por tipo de ordem. Ex.: COMPRA, VENDA"

mainPrompt :: Lang -> Prompt
mainPrompt lang = defaultPrompt lang

importPrompt :: Lang -> Prompt
importPrompt lang = defaultPrompt lang

importPrompt2 :: Lang -> Prompt
importPrompt2 "en" = "filename to import"
importPrompt2 _    = "nome do arquivo para importar"
-- end prompts
