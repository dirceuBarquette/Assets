module Lib
    ( getRecords
    , fBt
    , fBd
    , fBo
    , fTotal
    , fAvg
    , r2L
    , saveRegs2File
    , someFunc
    , fSummarize
    , summ2Lists
    , OpType(..)
    , Register(..)
    ) where

import System.IO ()
import Data.List (nub)
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

getTickers :: [Register] -> [Ticker]
getTickers rl = 
   let tks = fmap (\reg -> ticker reg) rl
   in nub tks

summ2Lists :: Summary -> [String]
summ2Lists = fmap (\(tk, (tot, qtt), avg, regs) -> 
                      tk ++ " "
                      ++ (show qtt) ++ " "
                      ++ (show $ truncate' tot 2) ++ " "
                      ++ (show $ truncate' avg 2) ++ "\n"
                      ++ ((take 80 $ repeat '=')::String) ++ "\n"
                      ++ (concat $
                           fmap (\reg ->
                              (show (date reg)) ++ " "
                              ++ (show (opType reg)) ++ " "
                              ++ (show (quantity reg)) ++ " "
                              ++ (show (value reg)) ++ "\n"
                         ) regs)
                  ) 

summarize :: [Register] -> Summary
summarize rec =
   let tks = getTickers rec
   in fmap (\tk -> let fbt = filterByTicker tk rec
                   in (tk, total fbt, fst $ avg fbt, fbt)) tks 

fSummarize :: Functor f => f [Register] -> f Summary
fSummarize = fmap summarize

getRecords :: FilePath -> IO [Register]
getRecords path = do
   contents <- readFile path 
   let lns = lines contents
   let ws = map words lns
   let regs = map (\x -> Register (x !! 2) 
                                  (read (head x) :: Day) 
                                  (read (x !! 1) :: OpType)
                                  (read (x !! 3) :: Int)
                                  (read (x !! 4) :: Double)) ws
   return regs

-- thx to
-- stackoverflow.com/questions/18723381/
-- rounding-to-specific-number-of-digits-in-haskell
truncate' :: Double -> Int -> Double
truncate' x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n

reg2Lists :: [Register] -> [String]
reg2Lists = map (\reg -> unwords $ 
                  [ show (date reg)
                  , show (opType reg)
                  , ticker reg
                  , show (quantity reg)
                  , show (value reg)
                  ])

r2L :: Functor f => f [Register] -> f [String]
r2L = fmap reg2Lists

saveRegs2File :: FilePath -> IO [String] -> IO ()
saveRegs2File path r2l = fmap unlines r2l >>= writeFile path

filterByOpType :: String -> [Register] -> [Register]
filterByOpType "" regs = regs
filterByOpType op regs = filter (\reg -> opType reg == (read op::OpType)) regs

fBo :: Functor f => String -> f [Register] -> f [Register]
fBo op = fmap (\x -> filterByOpType op x)

filterBetweenDates :: String -> [Register] -> [Register]
filterBetweenDates "" regs  = regs
filterBetweenDates dts regs = filter (\reg -> elem (date reg) range) regs
                                where
                                 wdts = words dts
                                 [dI, dF] = if length wdts == 2 then
                                              wdts
                                            else [dts, dts]
                                 range = [(read dI::Day)..(read dF::Day)]

fBd :: Functor f => String -> f [Register] -> f [Register]
fBd dts = fmap (\x -> filterBetweenDates dts x)

filterByTicker :: String -> [Register] -> [Register]
filterByTicker "all" regs = regs
filterByTicker tk    regs = filter (\reg -> elem (ticker reg) $ words tk) regs

fBt :: Functor f => String -> f [Register] -> f [Register]
fBt tk = fmap (\x -> filterByTicker tk x)

total :: [Register] -> (Double, Int)
total = foldl (\(v,q) reg -> 
                let amount = fromIntegral (quantity reg) * value reg
                in if opType reg == COMPRA then
                     (v + amount, q + (quantity reg))
                   else
                     (v - amount, q - (quantity reg)))
              (0,0)

fTotal :: Functor f => f [Register] -> f (Double, Int)
fTotal = fmap total

cv :: OpType -> (Double, Int) -> (Double, Int) -> (Double, Int)
cv op (pm,q) (v,qop) = (pmf, qf)
      where
         qf  = if op == COMPRA then
                  sum [q, qop]
               else subtract qop q
         pmf = 
            case op of
               COMPRA -> (sum [pm * (fromIntegral q), v * (fromIntegral qop)])
                           / fromIntegral qf
               VENDA  -> if not (qf == 0) then pm else 0
         
avg :: [Register] -> (Double, Int)
avg = foldl (\ (pm,q) reg-> 
               cv (opType reg) (pm, q) (value reg, quantity reg)
             ) (0, 0)

fAvg :: Functor f => f [Register] -> f (Double, Int)
fAvg = fmap avg

someFunc :: IO ()
someFunc = putStrLn "someFunc"
