{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.List
import Data.Time
import GHC.Stack (HasCallStack)
import Pdf
import System.Environment

main :: IO ()
main =
    do
        args <- getArgs
        case args of
            ["current", filename] -> do
                trans <- readPdfCurrent filename
                dumpTrans trans
            ["credit", filename] -> do
                trans <- readPdfCredit filename
                dumpTrans trans
            _ -> putStrLn "Usage: hsbc-pdf current FILENAME or hsbc-pdf credit FILENAME"
        return ()

-- ---------------------------------------------------------------------

-- | Generate hledger transactions
dumpTrans :: (HasCallStack) => [([ByteString], [Transaction])] -> IO ()
dumpTrans trans = do
    let allTrans = concatMap snd trans
    putStrLn $ "\"Date\",\"Detail\",\"Amount\""
    foldM_ fold_transaction Nothing allTrans
    return ()

fold_transaction :: (HasCallStack) => Maybe Day -> Transaction -> IO (Maybe Day)
fold_transaction acc transaction = do
    let acc' = case tDate transaction of
            Nothing -> acc
            Just d -> getDate d
    case acc' of
        Nothing -> return ()
        Just date -> do
            -- print date
            printTransaction date transaction
            return ()
    return acc'

getDate :: Date -> Maybe Day
getDate (Date d) = parseTimeM False defaultTimeLocale "%d %b %y" d

printTransaction :: Day -> Transaction -> IO ()
printTransaction day (Transaction ttype _tdate tdetail tamount _tbalance) = do
    case ttype of
        Credit -> do
            putStrLn
                ( show day
                    ++ ","
                    ++ detailAsStr ttype tdetail
                    ++ ","
                    ++ show (signedAmountAsStr True tamount)
                )
            return ()
        _ -> do
            putStrLn
                ( show day
                    ++ ","
                    ++ detailAsStr ttype tdetail
                    ++ ","
                    ++ show (signedAmountAsStr False tamount)
                )
            return ()
    return ()

detailAsStr :: TransactionType -> Detail -> String
detailAsStr tt (Detail ds) = show (transactionTypeAsStr tt ++ ":" ++ intercalate ":" ds)

transactionTypeAsStr :: TransactionType -> String
transactionTypeAsStr DirectDebit = "DD"
transactionTypeAsStr DebitRequest = "DR"
transactionTypeAsStr BankPayment = "BP"
transactionTypeAsStr OnlineBankPayment = "OBP"
transactionTypeAsStr StopOrder = "SO"
transactionTypeAsStr Visa = "VIS"
transactionTypeAsStr Atm = "ATM"
transactionTypeAsStr Credit = "CR"
transactionTypeAsStr CreditCardDebit = "CCD"
transactionTypeAsStr CreditCardCredit = "CCC"

signedAmountAsStr :: Bool -> Amount -> String
signedAmountAsStr False (Amount m) = m
signedAmountAsStr True (Amount m) = "-" ++ m
