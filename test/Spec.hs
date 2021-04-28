import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Time (Day)
import DataTypes (Register (..), OpType (..), Ticker, Summary)

import LibAssets ( getRecords
                 , fBt
                 , fBd
                 , fBo
                 , fTotal
                 , fAvg
                 , r2L
                 , saveRegs2File
                 , hasValidTicker
                 , hasValidCost
                 , hasValidQuantity
                 , hasValidDate
                 , hasValidOpType
                 ) 

rec = getRecords "test/examples/few_records.txt"

spec :: Spec
spec = do
   describe "Getting Records" $ do
      it "Importing records from tiny file" $ 
          getRecords "test/examples/tiny.txt" `shouldReturn`
            ([Register {ticker = "GRND3"
                       , date = read "2019-07-23"::Day
                       , opType = COMPRA
                       , quantity = 200
                       , value = 7.73
                       }] :: [Register])
      it "Importing from empty file" $
         getRecords "test/examples/empty.txt" `shouldReturn` ([] :: [Register])
      it "Importing from non existing file" $
         getRecords "test/examples/empty" `shouldThrow` anyIOException
      it "Importing from large file" $
         getRecords "test/examples/few_records.txt" `shouldReturn` (
            [ Register {ticker = "GRND3", date = read "2019-07-23"::Day
             , opType = COMPRA, quantity = 200, value = 7.73}
            , Register {ticker = "ABEV3", date = read "2019-08-01"::Day
             , opType = COMPRA, quantity = 100, value = 20.4}
            , Register {ticker = "GRND3", date = read "2019-08-05"::Day
             , opType = COMPRA, quantity = 100, value = 7.46}
            , Register {ticker = "EGIE3", date = read "2019-09-02"::Day
             , opType = COMPRA, quantity = 100, value = 44.79}
            , Register {ticker = "GRND3", date = read "2019-10-01"::Day
             , opType = VENDA, quantity = 100, value = 37.17}
            , Register {ticker = "ITUB3", date = read "2019-10-01"::Day
             , opType = COMPRA, quantity = 200, value = 29.87}
            , Register {ticker = "MDIA3", date = read "2019-10-01"::Day
             , opType = COMPRA, quantity = 200, value = 34.58}
            , Register {ticker = "WEGE3", date = read "2019-11-01"::Day
             , opType = COMPRA, quantity = 200, value = 26.22}
            , Register {ticker = "ABEV3", date = read "2019-11-01"::Day
             , opType = COMPRA, quantity = 200, value = 17.39}
            , Register {ticker = "WEGE3", date = read "2019-12-02"::Day
             , opType = COMPRA, quantity = 200, value = 30.36}
            , Register {ticker = "IRBR3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 39.37}
            , Register {ticker = "ITUB3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 29.74}
            , Register {ticker = "ABEV3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 18.04}
            , Register {ticker = "WEGE3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 40.64}
            , Register {ticker = "MDIA3", date = read "2020-02-10"::Day
             , opType = VENDA, quantity = 100, value = 34.36}
            , Register {ticker = "ABEV3", date = read "2020-02-10"::Day
             , opType = COMPRA, quantity = 100, value = 16.73}
            , Register {ticker = "GRND3", date = read "2020-02-10"::Day
             , opType = VENDA, quantity = 100, value = 35.45}
            , Register {ticker = "EGIE3", date = read "2020-02-28"::Day
             , opType = VENDA, quantity = 100, value = 31.9}
            ] :: [Register])

   describe "Filtering Ticker" $ do
      it "Filtering by one ticker at time" $ 
         fBt "WEGE3" rec `shouldReturn` (
            [ Register {ticker = "WEGE3", date = read "2019-11-01"::Day
             , opType = COMPRA, quantity = 200, value = 26.22}
            , Register {ticker = "WEGE3", date = read "2019-12-02"::Day
             , opType = COMPRA, quantity = 200, value = 30.36}
            , Register {ticker = "WEGE3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 40.64}
            ] :: [Register] )

      it "Filtering by one wrong ticker" $ 
         fBt "WEGE" rec `shouldReturn` ([] :: [Register])

      it "Filtering by an empty ticker field" $ 
         fBt "" rec `shouldReturn` ([] :: [Register])

      it "Filtering by two tickers at time" $ 
         fBt "WEGE3 EGIE3" rec `shouldReturn` (
            [ Register {ticker = "EGIE3", date = read "2019-09-02"::Day
             , opType = COMPRA, quantity = 100, value = 44.79}
            , Register {ticker = "WEGE3", date = read "2019-11-01"::Day
             , opType = COMPRA, quantity = 200, value = 26.22}
            , Register {ticker = "WEGE3", date = read "2019-12-02"::Day
             , opType = COMPRA, quantity = 200, value = 30.36}
            , Register {ticker = "WEGE3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 40.64}
            , Register {ticker = "EGIE3", date = read "2020-02-28"::Day
             , opType = VENDA, quantity = 100, value = 31.9}
            ] :: [Register] )

      it "Filtering by two tickers at time, but one is wrong" $ 
         fBt "WEGE EGIE3" rec `shouldReturn` (
            [ Register {ticker = "EGIE3", date = read "2019-09-02"::Day
             , opType = COMPRA, quantity = 100, value = 44.79}
            , Register {ticker = "EGIE3", date = read "2020-02-28"::Day
             , opType = VENDA, quantity = 100, value = 31.9}
            ] :: [Register] )

      it "Filtering by using 'all' as reserved word" $
         fBt "all" rec `shouldReturn` (
            [ Register {ticker = "GRND3", date = read "2019-07-23"::Day
             , opType = COMPRA, quantity = 200, value = 7.73}
            , Register {ticker = "ABEV3", date = read "2019-08-01"::Day
             , opType = COMPRA, quantity = 100, value = 20.4}
            , Register {ticker = "GRND3", date = read "2019-08-05"::Day
             , opType = COMPRA, quantity = 100, value = 7.46}
            , Register {ticker = "EGIE3", date = read "2019-09-02"::Day
             , opType = COMPRA, quantity = 100, value = 44.79}
            , Register {ticker = "GRND3", date = read "2019-10-01"::Day
             , opType = VENDA, quantity = 100, value = 37.17}
            , Register {ticker = "ITUB3", date = read "2019-10-01"::Day
             , opType = COMPRA, quantity = 200, value = 29.87}
            , Register {ticker = "MDIA3", date = read "2019-10-01"::Day
             , opType = COMPRA, quantity = 200, value = 34.58}
            , Register {ticker = "WEGE3", date = read "2019-11-01"::Day
             , opType = COMPRA, quantity = 200, value = 26.22}
            , Register {ticker = "ABEV3", date = read "2019-11-01"::Day
             , opType = COMPRA, quantity = 200, value = 17.39}
            , Register {ticker = "WEGE3", date = read "2019-12-02"::Day
             , opType = COMPRA, quantity = 200, value = 30.36}
            , Register {ticker = "IRBR3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 39.37}
            , Register {ticker = "ITUB3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 29.74}
            , Register {ticker = "ABEV3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 18.04}
            , Register {ticker = "WEGE3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 40.64}
            , Register {ticker = "MDIA3", date = read "2020-02-10"::Day
             , opType = VENDA, quantity = 100, value = 34.36}
            , Register {ticker = "ABEV3", date = read "2020-02-10"::Day
             , opType = COMPRA, quantity = 100, value = 16.73}
            , Register {ticker = "GRND3", date = read "2020-02-10"::Day
             , opType = VENDA, quantity = 100, value = 35.45}
            , Register {ticker = "EGIE3", date = read "2020-02-28"::Day
             , opType = VENDA, quantity = 100, value = 31.9}
            ] :: [Register])

   describe "Filtering by date" $ do
      it "Filtering between two dates" $ 
         fBd "2019-11-01 2020-02-03" rec `shouldReturn` (
            [ Register {ticker = "WEGE3", date = read "2019-11-01"::Day
             , opType = COMPRA, quantity = 200, value = 26.22}
            , Register {ticker = "ABEV3", date = read "2019-11-01"::Day
             , opType = COMPRA, quantity = 200, value = 17.39}
            , Register {ticker = "WEGE3", date = read "2019-12-02"::Day
             , opType = COMPRA, quantity = 200, value = 30.36}
            , Register {ticker = "IRBR3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 39.37}
            , Register {ticker = "ITUB3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 29.74}
            , Register {ticker = "ABEV3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 18.04}
            , Register {ticker = "WEGE3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 40.64}
            ] :: [Register] )

      it "Filtering by date by omitting the date field" $
         fBd "" rec `shouldReturn` (
            [ Register {ticker = "GRND3", date = read "2019-07-23"::Day
             , opType = COMPRA, quantity = 200, value = 7.73}
            , Register {ticker = "ABEV3", date = read "2019-08-01"::Day
             , opType = COMPRA, quantity = 100, value = 20.4}
            , Register {ticker = "GRND3", date = read "2019-08-05"::Day
             , opType = COMPRA, quantity = 100, value = 7.46}
            , Register {ticker = "EGIE3", date = read "2019-09-02"::Day
             , opType = COMPRA, quantity = 100, value = 44.79}
            , Register {ticker = "GRND3", date = read "2019-10-01"::Day
             , opType = VENDA, quantity = 100, value = 37.17}
            , Register {ticker = "ITUB3", date = read "2019-10-01"::Day
             , opType = COMPRA, quantity = 200, value = 29.87}
            , Register {ticker = "MDIA3", date = read "2019-10-01"::Day
             , opType = COMPRA, quantity = 200, value = 34.58}
            , Register {ticker = "WEGE3", date = read "2019-11-01"::Day
             , opType = COMPRA, quantity = 200, value = 26.22}
            , Register {ticker = "ABEV3", date = read "2019-11-01"::Day
             , opType = COMPRA, quantity = 200, value = 17.39}
            , Register {ticker = "WEGE3", date = read "2019-12-02"::Day
             , opType = COMPRA, quantity = 200, value = 30.36}
            , Register {ticker = "IRBR3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 39.37}
            , Register {ticker = "ITUB3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 29.74}
            , Register {ticker = "ABEV3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 18.04}
            , Register {ticker = "WEGE3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 40.64}
            , Register {ticker = "MDIA3", date = read "2020-02-10"::Day
             , opType = VENDA, quantity = 100, value = 34.36}
            , Register {ticker = "ABEV3", date = read "2020-02-10"::Day
             , opType = COMPRA, quantity = 100, value = 16.73}
            , Register {ticker = "GRND3", date = read "2020-02-10"::Day
             , opType = VENDA, quantity = 100, value = 35.45}
            , Register {ticker = "EGIE3", date = read "2020-02-28"::Day
             , opType = VENDA, quantity = 100, value = 31.9}
            ] :: [Register])

      it "Filtering by using just a date" $ 
         fBd "2019-11-01" rec `shouldReturn` (
            [ Register {ticker = "WEGE3", date = read "2019-11-01"::Day
             , opType = COMPRA, quantity = 200, value = 26.22}
            , Register {ticker = "ABEV3", date = read "2019-11-01"::Day
             , opType = COMPRA, quantity = 200, value = 17.39}
            ] :: [Register] )

      it "Filtering between two dates without records" $ 
         fBd "2020-02-04 2020-02-09" rec `shouldReturn` ([] :: [Register] )

      it "Filtering by using just a date without records" $ 
         fBd "2019-11-04" rec `shouldReturn` ([] :: [Register] )


   describe "Filtering by operation" $ do
      it "Filtering by COMPRA" $ 
         fBo "COMPRA" rec `shouldReturn` (
            [ Register {ticker = "GRND3", date = read "2019-07-23"::Day
             , opType = COMPRA, quantity = 200, value = 7.73}
            , Register {ticker = "ABEV3", date = read "2019-08-01"::Day
             , opType = COMPRA, quantity = 100, value = 20.4}
            , Register {ticker = "GRND3", date = read "2019-08-05"::Day
             , opType = COMPRA, quantity = 100, value = 7.46}
            , Register {ticker = "EGIE3", date = read "2019-09-02"::Day
             , opType = COMPRA, quantity = 100, value = 44.79}
            , Register {ticker = "ITUB3", date = read "2019-10-01"::Day
             , opType = COMPRA, quantity = 200, value = 29.87}
            , Register {ticker = "MDIA3", date = read "2019-10-01"::Day
             , opType = COMPRA, quantity = 200, value = 34.58}
            , Register {ticker = "WEGE3", date = read "2019-11-01"::Day
             , opType = COMPRA, quantity = 200, value = 26.22}
            , Register {ticker = "ABEV3", date = read "2019-11-01"::Day
             , opType = COMPRA, quantity = 200, value = 17.39}
            , Register {ticker = "WEGE3", date = read "2019-12-02"::Day
             , opType = COMPRA, quantity = 200, value = 30.36}
            , Register {ticker = "IRBR3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 39.37}
            , Register {ticker = "ITUB3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 29.74}
            , Register {ticker = "ABEV3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 18.04}
            , Register {ticker = "WEGE3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 40.64}
            , Register {ticker = "ABEV3", date = read "2020-02-10"::Day
             , opType = COMPRA, quantity = 100, value = 16.73}
            ] :: [Register])
            
            
      it "Filtering by VENDA" $ 
         fBo "VENDA" rec `shouldReturn` (
            [ Register {ticker = "GRND3", date = read "2019-10-01"::Day
             , opType = VENDA, quantity = 100, value = 37.17}
            , Register {ticker = "MDIA3", date = read "2020-02-10"::Day
             , opType = VENDA, quantity = 100, value = 34.36}
            , Register {ticker = "GRND3", date = read "2020-02-10"::Day
             , opType = VENDA, quantity = 100, value = 35.45}
            , Register {ticker = "EGIE3", date = read "2020-02-28"::Day
             , opType = VENDA, quantity = 100, value = 31.9}
            ] :: [Register])

      it "Filtering by omitting the operation field" $
         fBo "" rec `shouldReturn` (
            [ Register {ticker = "GRND3", date = read "2019-07-23"::Day
             , opType = COMPRA, quantity = 200, value = 7.73}
            , Register {ticker = "ABEV3", date = read "2019-08-01"::Day
             , opType = COMPRA, quantity = 100, value = 20.4}
            , Register {ticker = "GRND3", date = read "2019-08-05"::Day
             , opType = COMPRA, quantity = 100, value = 7.46}
            , Register {ticker = "EGIE3", date = read "2019-09-02"::Day
             , opType = COMPRA, quantity = 100, value = 44.79}
            , Register {ticker = "GRND3", date = read "2019-10-01"::Day
             , opType = VENDA, quantity = 100, value = 37.17}
            , Register {ticker = "ITUB3", date = read "2019-10-01"::Day
             , opType = COMPRA, quantity = 200, value = 29.87}
            , Register {ticker = "MDIA3", date = read "2019-10-01"::Day
             , opType = COMPRA, quantity = 200, value = 34.58}
            , Register {ticker = "WEGE3", date = read "2019-11-01"::Day
             , opType = COMPRA, quantity = 200, value = 26.22}
            , Register {ticker = "ABEV3", date = read "2019-11-01"::Day
             , opType = COMPRA, quantity = 200, value = 17.39}
            , Register {ticker = "WEGE3", date = read "2019-12-02"::Day
             , opType = COMPRA, quantity = 200, value = 30.36}
            , Register {ticker = "IRBR3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 39.37}
            , Register {ticker = "ITUB3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 29.74}
            , Register {ticker = "ABEV3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 18.04}
            , Register {ticker = "WEGE3", date = read "2020-02-03"::Day
             , opType = COMPRA, quantity = 100, value = 40.64}
            , Register {ticker = "MDIA3", date = read "2020-02-10"::Day
             , opType = VENDA, quantity = 100, value = 34.36}
            , Register {ticker = "ABEV3", date = read "2020-02-10"::Day
             , opType = COMPRA, quantity = 100, value = 16.73}
            , Register {ticker = "GRND3", date = read "2020-02-10"::Day
             , opType = VENDA, quantity = 100, value = 35.45}
            , Register {ticker = "EGIE3", date = read "2020-02-28"::Day
             , opType = VENDA, quantity = 100, value = 31.9}
            ] :: [Register])

   describe "Filtering by combining filters" $ do
      it "Filtering by 'WEGE3 ABEV3', 'COMPRA', '2019'" $ 
         (fBt "WEGE3 ABEV3" . fBd "2019-01-01 2019-12-31" . fBo "COMPRA")
          rec `shouldReturn` (
            [ Register {ticker = "ABEV3", date = read "2019-08-01"::Day
             , opType = COMPRA, quantity = 100, value = 20.4}
            , Register {ticker = "WEGE3", date = read "2019-11-01"::Day
             , opType = COMPRA, quantity = 200, value = 26.22}
            , Register {ticker = "ABEV3", date = read "2019-11-01"::Day
             , opType = COMPRA, quantity = 200, value = 17.39}
            , Register {ticker = "WEGE3", date = read "2019-12-02"::Day
             , opType = COMPRA, quantity = 200, value = 30.36}
            ] :: [Register])

      it "Cost average, filtering by 'WEGE3', 'COMPRA', '2019'" $ 
         (fAvg $ (fBt "WEGE3" . fBd "2019-01-01 2019-12-31" . fBo "COMPRA")
          rec) `shouldReturn` ((28.29,400) :: (Double, Int))

      it "Cost average by filtering a daytrade, same cost" $
         (fAvg $ fBt "EGIE3" rec) `shouldReturn` ((0.0,0) :: (Double, Int))
      
      it "Total wallet!" $
         fTotal rec `shouldReturn` ((37059.0,1600) :: (Double, Int))

   describe "Dealing with Registers to save" $ do
      it "Registers becoming Strings (ready to save)" $
         r2L rec `shouldReturn` (
            ["2019-07-23 COMPRA GRND3 200 7.73"
            ,"2019-08-01 COMPRA ABEV3 100 20.4"
            ,"2019-08-05 COMPRA GRND3 100 7.46"
            ,"2019-09-02 COMPRA EGIE3 100 44.79"
            ,"2019-10-01 VENDA GRND3 100 37.17"
            ,"2019-10-01 COMPRA ITUB3 200 29.87"
            ,"2019-10-01 COMPRA MDIA3 200 34.58"
            ,"2019-11-01 COMPRA WEGE3 200 26.22"
            ,"2019-11-01 COMPRA ABEV3 200 17.39"
            ,"2019-12-02 COMPRA WEGE3 200 30.36"
            ,"2020-02-03 COMPRA IRBR3 100 39.37"
            ,"2020-02-03 COMPRA ITUB3 100 29.74"
            ,"2020-02-03 COMPRA ABEV3 100 18.04"
            ,"2020-02-03 COMPRA WEGE3 100 40.64"
            ,"2020-02-10 VENDA MDIA3 100 34.36"
            ,"2020-02-10 COMPRA ABEV3 100 16.73"
            ,"2020-02-10 VENDA GRND3 100 35.45"
            ,"2020-02-28 VENDA EGIE3 100 31.9"
            ] :: [String])
   
   describe "Dealing with wrong fields and entries" $ do                        
       it "Validating a entire wrong file" $                                     
          getRecords "test/examples/entirely_wrong_fields.txt"                   
            `shouldReturn` ([] :: [Register])
            
       it "Validating valid ticker (ABEV3)" $                                     
          hasValidTicker "ABEV3"                   
            `shouldBe` (True)

       it "Validating wrong ticker (testtttt\'\'\")" $                                     
          hasValidTicker "testtttt''\""                   
            `shouldBe` (False)

       it "Validating wrong ticker (^`*!@#_&<)" $                                     
          hasValidTicker "^`*!@#_&<"                   
            `shouldBe` (False)

       it "Validating wrong ticker (\"\")" $                                     
          hasValidTicker ""                   
            `shouldBe` (False)

       it "Validating wrong ticker (\"        \")" $                                     
          hasValidTicker "        "                   
            `shouldBe` (False)

       it "Validating valid date (2020-10-01)" $                                     
          hasValidDate "2020-10-01"                   
            `shouldBe` (True)

       it "Validating wrong date (2020-0101)" $                                     
          hasValidDate "2020-0101"                   
            `shouldBe` (False)

       it "Validating wrong date (20200101)" $                                     
          hasValidDate "20200101"                   
            `shouldBe` (False)

       it "Validating wrong date (01-01-2020)" $                                     
          hasValidDate "01-01-2020"                   
            `shouldBe` (False)

       it "Validating wrong date (202001o1)" $                                     
          hasValidDate "202001o1"                   
            `shouldBe` (False)

       it "Validating wrong date (2020'01'01)" $                                     
          hasValidDate "2020'01'01"                   
            `shouldBe` (False)

       it "Validating wrong date (\"\")" $                                     
          hasValidDate ""                   
            `shouldBe` (False)

       it "Validating wrong date (01/01/2020)" $                                     
          hasValidDate "01/01/2020"                   
            `shouldBe` (False)

       it "Validating wrong date (2300-01-01)" $                                     
          hasValidDate "2300-01-01"                   
            `shouldBe` (False)

       it "Validating wrong date (2020-13-01)" $                                     
          hasValidDate "2020-13-01"                   
            `shouldBe` (False)

       it "Validating valid operation (COMPRA)" $                                     
          hasValidOpType "COMPRA"                   
            `shouldBe` (True)

       it "Validating valid operation (VENDA)" $                                     
          hasValidOpType "VENDA"                   
            `shouldBe` (True)

       it "Validating wrong operation (VEND)" $                                     
          hasValidOpType "VEND"                   
            `shouldBe` (False)

       it "Validating wrong operation (COMPR)" $                                     
          hasValidOpType "COMPR"                  
            `shouldBe` (False)

       it "Validating wrong operation (\"\")" $                                     
          hasValidOpType ""                  
            `shouldBe` (False)

       it "Validating wrong operation (\" \")" $                                     
          hasValidOpType " "                  
            `shouldBe` (False)

       it "Validating valid quantity (100)" $                                     
          hasValidQuantity "100"                  
            `shouldBe` (True)

       it "Validating wrong quantity (-100)" $                                     
          hasValidQuantity "-100"                  
            `shouldBe` (False)

       it "Validating wrong quantity (100e1800)" $                                     
          hasValidQuantity "100e1800"                  
            `shouldBe` (False)

       it "Validating wrong quantity (\"\")" $                                     
          hasValidQuantity ""                  
            `shouldBe` (False)

       it "Validating valid cost (10.50)" $                                     
          hasValidCost "10.50"                  
            `shouldBe` (True)

       it "Validating valid cost (10.00)" $                                     
          hasValidCost "10.00"                  
            `shouldBe` (True)

       it "Validating valid cost (10)" $                                     
          hasValidCost "10"                  
            `shouldBe` (True)

       it "Validating wrong cost (\"\")" $                                     
          hasValidCost ""                  
            `shouldBe` (False)

       it "Validating wrong cost (10,00)" $                                     
          hasValidCost "10,00"                  
            `shouldBe` (False)

       it "Validating wrong cost (10'00)" $                                     
          hasValidCost "10'00"                  
            `shouldBe` (False)

       it "Validating wrong cost (-1)" $                                     
          hasValidCost "-1"                  
            `shouldBe` (False)

       it "Validating wrong cost (1o.00)" $                                     
          hasValidCost "1o.00"                  
            `shouldBe` (False)

       it "Validating wrong cost (,10)" $                                     
          hasValidCost ",10"                  
            `shouldBe` (False)

       it "Validating wrong cost (10,)" $                                     
          hasValidCost "10,"                  
            `shouldBe` (False)

       it "Validating wrong cost (10^10)" $                                     
          hasValidCost "10^10"                  
            `shouldBe` (False)

main :: IO ()
main = hspec $ spec
