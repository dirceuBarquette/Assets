{-|                                                                                
Module      : Main
Description : User Interface functions
Copyright   : 2021 Dirceu Barquette
License     : BSD3
Maintainer  : dirceu.barquette@gmail.com

-}
module Main where

import System.Environment
   ( getArgs )
import System.IO
   ( hSetBuffering
   , stdout
   , BufferMode( NoBuffering ) 
   )
import ATui
   ( loadMain )
import AGui
   ( mainWindow )

main :: IO ()
main = do
   args <- getArgs
   case args of
      [tui] -> do hSetBuffering stdout NoBuffering
                  loadMain 9
      _     -> do mainWindow
