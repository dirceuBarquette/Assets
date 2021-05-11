{-|                                                                                
Module      : DataTypes
Description : data structures
Copyright   : 2021 Dirceu Barquette
License     : BSD3
Maintainer  : dirceu.barquette@gmail.com

This module presents the main data structure used throughout this application.
-}

module DataTypes
   ( OpType (..)
   , Register (..)
   , Ticker
   , Summary
   ) where

import Data.Time (Day)

-- | The 'Ticker' represents the object of the transaction.
-- This is always a string.
type Ticker  = String

-- | The 'Summary' type imposes a rigid output format
type Summary = [(Ticker,(Double, Int), Double, [Register])]

-- | The 'Register' data type represents a stock market transaction
data Register  = Register { ticker   :: Ticker 
                          , date     :: Day
                          , opType   :: OpType 
                          , quantity :: Int
                          , value    :: Double
                          } deriving (Show, Eq)

-- | A stock market transaction, 'OpType', can be represented at 
-- least BUY or SELL
data OpType = COMPRA | VENDA deriving (Show, Read, Eq)
