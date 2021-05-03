module CommandLine
   ( showScreen
   ) where

type Menu     = [String]
type Prompt   = String
type Screen   = (Int, (Menu, Prompt))

showScreen :: Int -> IO ()
showScreen i = putStr $ theMenu ++ thePrompt 
   where (Just (menu, prompt)) = lookup i screens
         theMenu   = unlines menu
         thePrompt = prompt

screens :: [Screen]
screens = [ exitScreen 
          , mainScreen
          , importScreen
          , importScreen2
          , filterScreen
          , filterScreen2
          , file2BeFilteredScreen
          , filterByTickerScreen
          , filterByDateScreen
          , filterByOperScreen
          , summarizeScreen
          , summarizeScreen2
          ]

-- begin screens
exitScreen :: Screen
exitScreen = (0, (exitMenu, exitPrompt))

filterScreen :: Screen
filterScreen = (2, (filterMenu, filterPrompt))

summarizeScreen :: Screen
summarizeScreen = (3, (summarizeMenu, summarizePrompt))

filterScreen2 :: Screen
filterScreen2 = (21, (filterMenu2, filterPrompt2))

summarizeScreen2 :: Screen
summarizeScreen2 = (26, (summarizeMenu2, summarizePrompt2))

filterByTickerScreen :: Screen
filterByTickerScreen = (23, (filterByTickerMenu, filterByTickerPrompt))

filterByDateScreen :: Screen
filterByDateScreen = (24, (filterByDateMenu, filterByDatePrompt))

filterByOperScreen :: Screen
filterByOperScreen = (25, (filterByOperMenu, filterByOperPrompt))

file2BeFilteredScreen :: Screen
file2BeFilteredScreen = (22, (file2BeFilteredMenu, file2BeFilteredPrompt))

mainScreen :: Screen
mainScreen = (9, (mainMenu, mainPrompt))

importScreen :: Screen
importScreen = (1, (importMenu, importPrompt))

importScreen2 :: Screen
importScreen2 = (11, (importMenu2, importPrompt2))
-- end screens

-- begin menus
exitMenu :: Menu
exitMenu = []

filterMenu :: Menu
filterMenu = ["1 - Selecionar arquivo", "0 - Voltar"]

summarizeMenu :: Menu
summarizeMenu = ["1 - Selecionar arquivo", "0 - Voltar"]

summarizeMenu2 :: Menu
summarizeMenu2 = ["0 - Voltar"]

filterMenu2 :: Menu
filterMenu2 = ["0 - Voltar"]

filterByTickerMenu :: Menu
filterByTickerMenu = []

filterByDateMenu :: Menu
filterByDateMenu = []

filterByOperMenu :: Menu
filterByOperMenu = []

file2BeFilteredMenu :: Menu
file2BeFilteredMenu = 
   [ "1 - Filtrar por ticker(s). Ex.: ABCD3, ABCD3 EFGH4 KLMN11"
   , "2 - Filtrar por data(s). Ex.: 2019-01-01 2019-12-31, 2020-04-01"
   , "3 - Filtrar por tipo de ordem. Ex.: COMPRA, VENDA"
   , "0 - Voltar"
   ]

importMenu :: Menu
importMenu = ["1 - Importar", "0 - Voltar"]

importMenu2 :: Menu
importMenu2 = []

mainMenu :: Menu
mainMenu = ["1 - Importar", "2 - Filtrar", "3 - Sumarizar","0 - Sair"]
-- end menus

-- begin prompts
defaultPrompt :: String
defaultPrompt = "digite uma opção > "

exitPrompt :: Prompt
exitPrompt = "Até a próxima!"

filterPrompt :: Prompt
filterPrompt = defaultPrompt

summarizePrompt :: Prompt
summarizePrompt = defaultPrompt

summarizePrompt2 :: Prompt
summarizePrompt2 = defaultPrompt

filterPrompt2 :: Prompt
filterPrompt2 = defaultPrompt

file2BeFilteredPrompt :: Prompt
file2BeFilteredPrompt = defaultPrompt

filterByTickerPrompt :: Prompt
filterByTickerPrompt =
   "Filtrar por ticker(s). Ex.: ABCD3, ABCD3 EFGH4 KLMN11 > "

filterByDatePrompt :: Prompt
filterByDatePrompt =
   "Filtrar por data(s). Ex.: 2019-01-01 2019-12-31, 2020-04-01 > "

filterByOperPrompt :: Prompt
filterByOperPrompt =
   "Filtrar por tipo de ordem. Ex.: COMPRA, VENDA > "

mainPrompt :: Prompt
mainPrompt = defaultPrompt

importPrompt :: Prompt
importPrompt = defaultPrompt

importPrompt2 :: Prompt
importPrompt2 = "nome do arquivo para importar > "
-- end prompts
