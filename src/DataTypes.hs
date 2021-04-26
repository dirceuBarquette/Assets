module DataTypes
   ( OpType (..)
   , Register (..)
   , Ticker
   , Summary
   ) where

import Data.Time (Day)

type Ticker  = String
type Summary = [(Ticker,(Double, Int), Double, [Register])]

data OpType = COMPRA | VENDA deriving (Show, Read, Eq)

data Register  = Register { ticker   :: Ticker 
                          , date     :: Day
                          , opType   :: OpType 
                          , quantity :: Int
                          , value    :: Double
                          } deriving (Show, Eq)
