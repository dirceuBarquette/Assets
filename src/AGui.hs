{-|                                                                                
Module      : Graphics 
Description : Build a graphical user interface
Copyright   : 2021 Dirceu Barquette
License     : BSD3
Maintainer  : dirceu.barquette@gmail.com
-}
module AGui
   ( mainWindow ) where

import Control.Monad
   ( void
   , forM_
   )
import System.IO
   ( hFlush
   , stdout
   )
import System.Directory
   ( getCurrentDirectory
   , renameFile
   , doesFileExist
   )
import System.Glib.UTFString
   ( GlibString )
import Control.Concurrent
   ( threadDelay )
import Control.Monad.IO.Class
   ( liftIO )
import Graphics.UI.Gtk
   ( Window
   , initGUI
   , windowNew
   , windowTitle
   , windowDefaultHeight
   , windowDefaultWidth
   , widgetShowAll
   , mainGUI
   , mainQuit
   , deleteEvent
   , buttonActivated
   , hSeparatorNew
   , vSeparatorNew
   , on
   , set
   , VBox
   , vBoxNew
   , hBoxNew
   , widgetExpand
   , widgetHExpand
   , widgetVExpand
   , hButtonBoxNew
   , VButtonBox
   , ButtonBoxStyle( ButtonboxStart )
   , buttonBoxSetLayout
   , vButtonBoxNew
   , buttonNewWithLabel
   , buttonGetLabel
   , containerAdd
   , containerBorderWidth
   , containerRemove
   , containerChild
   , containerGetChildren
   , textBufferNew
   , TextBuffer
   , textViewEditable
   , textViewNewWithBuffer
   , textBufferSetText
   , scrolledWindowNew
   , labelNew
   , viewportNew
   , adjustmentNew
   , vSeparatorNew
   , widgetOpacity
   , entryNew
   , Entry
   , entryEditable
   , entryNewWithBuffer
   , entryBufferNew
   , EntryBuffer
   , entryBufferInsertText
   , entryBufferDeleteText
   , entryGetText
   , entrySetText
   , Button
   , ButtonClass
   , ButtonsType( ButtonsOk )
   , widgetDestroy
   , dialogRun
   , dialogAddButton
   , MessageType
      ( MessageQuestion
      , MessageError
      , MessageInfo
      )
   , DialogFlags( DialogDestroyWithParent )
   , DialogFlags( DialogModal )
   , messageDialogNew
   , fileChooserGetFilename
   , fileChooserDialogNew
   , FileChooserAction
      ( FileChooserActionSave
      , FileChooserActionOpen
      )
   , fileChooserSetFilename
   , fileChooserSetCurrentFolder
   , ResponseId
      ( ResponseCancel
      , ResponseDeleteEvent
      , ResponseOk
      , ResponseApply
      )
   , ButtonsType( ButtonsNone )
   , AttrOp( (:=))
   )
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Abstract.Box
import Data.Time
   ( getZonedTime
   , formatTime
   , defaultTimeLocale
   , ZonedTime
   )
import Data.List
   ( isSuffixOf
   )
import Data.Char
   ( toUpper
   )
import DataTypes
   ( Register
   )
import Reports
   ( reg2Lists
   , summ2Lists
   , fSummarize
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

mainWindow :: IO ()
mainWindow = do
   void initGUI

   mainBox            <- hBoxNew False 0
   set mainBox [boxSpacing := 1, widgetExpand := True]

   window             <- windowNew
   set window [ windowDefaultWidth := 800, windowDefaultHeight := 400
              , containerBorderWidth := 2, containerChild := mainBox
              , windowTitle := "Assets"
              ]

   importBox          <- vBoxNew False 2
   set importBox [ widgetExpand := True]
   sep1               <- vSeparatorNew
   filterBox          <- vBoxNew False 2
   set filterBox [ widgetOpacity := 0.9, widgetExpand := True ]
   sep2               <- vSeparatorNew
   viewBox            <- vBoxNew False 2
   set viewBox [ widgetExpand := True]

   boxPackStart mainBox importBox PackGrow 0
   boxPackStart mainBox sep1      PackNatural 0
   boxPackStart mainBox filterBox PackGrow 0
   boxPackStart mainBox sep2      PackNatural 0
   boxPackStart mainBox viewBox   PackGrow 0

   importBtBtBox      <- hButtonBoxNew
   set importBtBtBox [widgetExpand := False]

   importBtn          <- buttonNewWithLabel "Importar"
   set importBtn []
   containerAdd importBtBtBox importBtn

   sepImport          <- hSeparatorNew

   filesScrolled      <- scrolledWindowNew Nothing Nothing
   set filesScrolled [widgetExpand := True]

   adj                <- adjustmentNew 0 0 0 0 0 0
   filesViewport      <- viewportNew adj adj
   containerAdd filesScrolled filesViewport

   filesBox           <- vBoxNew True 0
   set filesBox [ widgetHExpand := True
                , widgetVExpand := True
                ]
   containerAdd filesViewport filesBox

   boxPackStart importBox importBtBtBox PackNatural 0
   boxPackStart importBox sepImport     PackNatural 0
   boxPackStart importBox filesScrolled PackGrow 0

   filterBtBtBox      <- hButtonBoxNew
   set filterBtBtBox [widgetExpand := False]

   filterBtn          <- buttonNewWithLabel "Filtrar"
   set filterBtn []
   containerAdd filterBtBtBox filterBtn

   sepFilter          <- hSeparatorNew

   filtersBox         <- vBoxNew True 2
   set filtersBox [ widgetHExpand := True
                  , widgetVExpand := True
                  ]

   firstBoxFilterBox  <- vBoxNew True 2
   set firstBoxFilterBox [ widgetHExpand := True, widgetVExpand := True ]

   secondBoxFilterBox <- vBoxNew True 2
   set secondBoxFilterBox [ widgetHExpand := True, widgetVExpand := True ]

   thirdBoxFilterBox  <- vBoxNew True 2
   set thirdBoxFilterBox [ widgetHExpand := True, widgetVExpand := True ]

   fbtLabel           <- labelNew (Just "Filtro por Ticker")
   tickerFilter       <- entryNew

   boxPackStart firstBoxFilterBox fbtLabel PackNatural 0
   boxPackStart firstBoxFilterBox tickerFilter PackNatural 0

   dbtLabel           <- labelNew (Just "Filtro por Data")
   dateFilter         <- entryNew

   boxPackStart secondBoxFilterBox dbtLabel   PackNatural 0
   boxPackStart secondBoxFilterBox dateFilter PackNatural 0

   dboLabel           <- labelNew (Just "Filtro por Operação")
   opFilter           <- entryNew

   boxPackStart thirdBoxFilterBox dboLabel   PackNatural 0
   boxPackStart thirdBoxFilterBox opFilter PackNatural 0

   boxPackStart filtersBox firstBoxFilterBox  PackNatural 0
   boxPackStart filtersBox secondBoxFilterBox PackNatural 0
   boxPackStart filtersBox thirdBoxFilterBox  PackNatural 0

   boxPackStart filterBox filterBtBtBox PackNatural 0
   boxPackStart filterBox sepFilter     PackNatural 0
   boxPackStart filterBox filtersBox    PackGrow 0
--
   viewBtBtBox        <- hButtonBoxNew
   set viewBtBtBox [widgetExpand := False]

   renameBtn          <- buttonNewWithLabel "Renomear"
   set renameBtn []

   discardBtn         <- buttonNewWithLabel "Descartar"
   set discardBtn []

   summarizeBtn       <- buttonNewWithLabel "Sumarizar"
   set summarizeBtn []

   exitBtn            <- buttonNewWithLabel "Sair"
   set exitBtn []

   boxPackStart viewBtBtBox renameBtn    PackNatural 0
   boxPackStart viewBtBtBox discardBtn   PackNatural 0
   boxPackStart viewBtBtBox summarizeBtn PackNatural 0
   boxPackStart viewBtBtBox exitBtn      PackNatural 0

   sepView            <- hSeparatorNew

   viewsBox           <- vBoxNew False 2
   set viewsBox [ widgetHExpand := True
                , widgetVExpand := True
                ]

   activeFile         <- entryBufferNew (Just "")
   viewBuffer         <- textBufferNew Nothing

   firstBoxViewsBox   <- vBoxNew True 0
   set firstBoxViewsBox [ widgetHExpand := True
                        , widgetVExpand := False
                        ]
   fileInUse          <- entryNewWithBuffer activeFile
   set fileInUse [ widgetExpand := False, entryEditable := False ]
   boxPackStart firstBoxViewsBox fileInUse PackNatural 0

   secondBoxViewsBox  <- vBoxNew True 2
   set secondBoxViewsBox [ widgetHExpand := True
                         , widgetVExpand := True
                         ]

   contentScrolled    <- scrolledWindowNew Nothing Nothing
   set contentScrolled [ widgetVExpand := True ]
   containerAdd secondBoxViewsBox contentScrolled

   content            <- textViewNewWithBuffer viewBuffer
   set content [ widgetVExpand := True, textViewEditable := False ]
   containerAdd contentScrolled content

   boxPackStart viewsBox firstBoxViewsBox  PackNatural 1
   boxPackStart viewsBox secondBoxViewsBox PackGrow 1

   boxPackStart viewBox viewBtBtBox PackNatural 0
   boxPackStart viewBox sepView     PackNatural 0
   boxPackStart viewBox viewsBox    PackGrow 0

   insertFileListButtonBox (filesBox,viewBuffer,activeFile)

   doAction destroyWin exitBtn
   doAction (importFile2bFiltered (window,filesBox,viewBuffer,activeFile))
      importBtn
   doAction
      (filtering (window,filesBox,viewBuffer,activeFile)
      [fileInUse,tickerFilter,dateFilter,opFilter]) filterBtn
   doAction (runDiscardDialog (window,filesBox,viewBuffer,activeFile)
      fileInUse) discardBtn
   doAction (runRenameDialog (window,filesBox,viewBuffer,activeFile)
      fileInUse) renameBtn
   doAction (summarizing (window,filesBox,viewBuffer,activeFile)
      fileInUse) summarizeBtn

   window `on` deleteEvent $ do
      liftIO mainQuit
      return False
   widgetShowAll window
   mainGUI

summarizing :: (Window,VBox,TextBuffer,EntryBuffer) -> Entry -> IO ()
summarizing (window,parent,viewBuffer,activeFile) fileInUse = do
   filename <- entryGetText fileInUse
   let msg = "Selecione um arquivo primeiro!"
   if null filename then opNotAllowedDialog window msg
   else do
      let rec = getRecords filename
      let fsumm = fSummarize rec
      let s2l = s2L fsumm
      now <- getZonedTime
      let summarized = makePath ("summary-",".txt") now
      saveRegs2File summarized s2l
      let msg = "Arquivo " ++ summarized ++ " criado com sucesso!"
      o <- fmap unlines s2l
      textBufferSetText viewBuffer o
      setActiveFile activeFile summarized
      dialog <- messageDialogNew
                  (Just window )
                  [ DialogModal, DialogDestroyWithParent]
                  MessageInfo ButtonsNone msg
      dialogAddButton dialog "Ok" ResponseOk
      res <- dialogRun dialog
      widgetDestroy dialog
   return ()

filtering :: (Window,VBox,TextBuffer,EntryBuffer) -> [Entry] -> IO ()
filtering (window,parent,viewBuffer,activeFile)
            [fileInUse,tickerFilter,dateFilter,opFilter] = do
   filename <- entryGetText fileInUse
   let msg = "Selecione um arquivo primeiro!"
   if null filename then opNotAllowedDialog window msg
   else do
      let rec = getRecords filename
      let filtered = getOpFilter opFilter
                     $ getDateFilter dateFilter
                     $ getTickerFilter tickerFilter rec
      let r2l = r2L filtered
      now <- getZonedTime
      let imported = makePath ("filtered-",".flt") now
      saveRegs2File imported r2l
      pause
      insertFileListButtonBox (parent,viewBuffer,activeFile)
      o <-  fmap unlines r2l
      textBufferSetText viewBuffer o
      setActiveFile activeFile imported
   return ()

getOpFilter :: Entry -> IO [DataTypes.Register] -> IO [DataTypes.Register]
getOpFilter op rec = do
   opp <- entryGetText op
   fBo (fmap toUpper opp) rec

getTickerFilter :: Entry -> IO [DataTypes.Register] -> IO [DataTypes.Register]
getTickerFilter tickers rec = do
   tks <- entryGetText tickers
   if null tks then rec else fBt (fmap toUpper tks) rec

getDateFilter :: Entry -> IO [DataTypes.Register] -> IO [DataTypes.Register]
getDateFilter dates rec = do
   dts <- entryGetText dates
   fBd dts rec

setActiveFile :: GlibString string => EntryBuffer -> string -> IO ()
setActiveFile activeFile label = do
   entryBufferDeleteText activeFile 0 (-100)
   entryBufferInsertText activeFile 0 label
   return ()

showContent :: (Button,TextBuffer,EntryBuffer) -> IO ()
showContent (btn,viewBuffer,activeFile) = do
   label <- buttonGetLabel btn
   rec <- fmap unlines $ r2L $ getRecords label
   textBufferSetText viewBuffer rec
   setActiveFile activeFile label
   return ()

insertFileListButtonBox :: (VBox,TextBuffer,EntryBuffer) -> IO ()
insertFileListButtonBox (parent,viewBuffer,activeFile) = do
   removeFileListButtonBox parent
   ffList <- filteredFilesInCurdir (isSuffixOf ".flt")
   fileButtonBox <- mkFileListButtonBox
   buttonBoxBuilded <- buildFileListButtonBox
                        (fileButtonBox,viewBuffer,activeFile) ffList
   containerAdd parent buttonBoxBuilded
   widgetShowAll parent

removeFileListButtonBox :: VBox -> IO ()
removeFileListButtonBox parent = do
   child <- containerGetChildren parent
   forM_ child (containerRemove parent)

buildFileListButtonBox :: (VButtonBox,TextBuffer,EntryBuffer)
                       -> [FilePath] -> IO VButtonBox
buildFileListButtonBox (buttonBox,viewBuffer,activeFile) paths = do
   mapM_ (\x -> do
            btn <- buttonNewWithLabel x
            set btn []
            doAction (showContent (btn,viewBuffer,activeFile)) btn
            boxPackStart buttonBox btn PackGrow 2
      ) paths
   return buttonBox

mkFileListButtonBox :: IO VButtonBox
mkFileListButtonBox = do
   bb <- vButtonBoxNew
   boxSetSpacing bb 5
   buttonBoxSetLayout bb ButtonboxStart
   set bb []
   return bb

doAction :: ButtonClass b => IO () -> b -> IO b
doAction action btn = do
   btn `on` buttonActivated $ do
      action
   return btn

destroyWin :: IO ()
destroyWin = do
   liftIO mainQuit
   return ()

importFile2bFiltered :: (Window,VBox,TextBuffer,EntryBuffer) -> IO ()
importFile2bFiltered (window,parent,viewBuffer,activeFile) = do
   cwd <- getCurrentDirectory
   dialog <- fileChooserDialogNew (Just "importar") (Just window)
      FileChooserActionOpen [("Cancelar", ResponseCancel),("OK", ResponseOk)]
   fileChooserSetCurrentFolder dialog cwd
   res <- dialogRun dialog
   case res of
      ResponseDeleteEvent -> widgetDestroy dialog
      ResponseOk -> do
         filename <- fileChooserGetFilename dialog
         case filename of
            Nothing -> widgetDestroy dialog
            Just path -> do
               let rec = getRecords path
               let r2l = r2L rec
               now <- getZonedTime
               let imported = makePath ("imported-",".flt") now
               saveRegs2File imported r2l
               insertFileListButtonBox (parent,viewBuffer,activeFile)
               widgetDestroy dialog
      ResponseCancel -> widgetDestroy dialog
   return ()

discardFile :: (VBox,TextBuffer,EntryBuffer) -> Entry -> IO ()
discardFile (parent,viewBuffer,activeFile) fileInUse = do
   filename <- entryGetText fileInUse
   cwd <- getCurrentDirectory
   let path = cwd ++ "/" ++ filename
   renameFile path $ path ++ "-discarded"
   textBufferSetText viewBuffer ""
   setActiveFile activeFile ""
   insertFileListButtonBox (parent,viewBuffer,activeFile)
   return ()

opNotAllowedDialog :: Window -> String -> IO ()
opNotAllowedDialog window msg = do
   dialog <- messageDialogNew
               (Just window )
               [ DialogModal, DialogDestroyWithParent ]
               MessageError ButtonsOk msg
   dialogRun dialog
   widgetDestroy dialog

runRenameDialog :: (Window,VBox,TextBuffer,EntryBuffer) -> Entry -> IO ()
runRenameDialog (window,parent,viewBuffer,activeFile) fileInUse = do
   filename <- entryGetText fileInUse
   let msg = "Selecione um arquivo primeiro!"
   if null (filename ++ "") then opNotAllowedDialog window msg
   else do
      dialog <- fileChooserDialogNew (Just "Renomear") (Just window)
         FileChooserActionSave [("Cancelar", ResponseCancel)
                               ,("Aplicar", ResponseApply)]
      fileChooserSetFilename dialog filename
      res <- dialogRun dialog
      case res of
         ResponseDeleteEvent -> widgetDestroy dialog
         ResponseCancel -> widgetDestroy dialog
         ResponseApply  -> do
            newFilename <- fileChooserGetFilename dialog
            case newFilename of
               Nothing -> widgetDestroy dialog
               Just path -> do
                  renameFile filename path
                  ffList <- filteredFilesInCurdir (isSuffixOf ".flt")
                  let searched = [x |x <- ffList,x `isSuffixOf` path]
                  forM_ searched $ do
                                    setActiveFile activeFile
                                    entrySetText fileInUse
                  insertFileListButtonBox (parent,viewBuffer,activeFile)
                  widgetDestroy dialog
      return ()

runDiscardDialog :: (Window,VBox,TextBuffer,EntryBuffer) -> Entry -> IO ()
runDiscardDialog (window,parent,viewBuffer,activeFile) fileInUse = do
   filename <- entryGetText fileInUse
   let selFirst = "Selecione um arquivo primeiro!"
   if null filename then opNotAllowedDialog window selFirst
   else do
      let msg = "Deseja descartar o arquivo " ++ filename ++ "?"
      dialog <- messageDialogNew
                  (Just window )
                  [ DialogModal, DialogDestroyWithParent]
                  MessageQuestion ButtonsNone msg
      dialogAddButton dialog "Cancelar" ResponseCancel
      dialogAddButton dialog "Descartar" ResponseApply
      res <- dialogRun dialog
      case res of
         ResponseCancel -> widgetDestroy dialog
         ResponseApply  -> do
            discardFile (parent,viewBuffer,activeFile) fileInUse
            widgetDestroy dialog
   return ()

makePath :: (String,String) -> ZonedTime -> String
makePath (prefix,suffix) =
   formatTime defaultTimeLocale format
      where format = prefix ++ "%Y-%m-%d.%H%M%S" ++ suffix

pause :: IO ()
pause = do
  hFlush stdout
  -- 1 second pause
  threadDelay 1000000
