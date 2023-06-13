{-# LANGUAGE OverloadedStrings #-}

module Pdf (
    readPdfCurrent,
    readPdfCredit,
    Transaction (..),
    TransactionType (..),
    Date (..),
    Detail (..),
    Amount (..),
    Balance (..),
    debug,
) where

import Control.Monad
import Data.ByteString.UTF8
import Data.List
import Data.Maybe (isJust, mapMaybe)
import Data.Scientific (Scientific)
import Data.Time
import Debug.Trace
import GHC.Stack (HasCallStack)
import Pdf.Content.Ops
import Pdf.Document
import Text.Parsec
import Text.Parsec.ByteString qualified as PB
import Text.Parsec.Pos

-- ---------------------------------------------------------------------

readPdfCurrent :: FilePath -> IO [([ByteString], [Transaction])]
readPdfCurrent fileName = do
    withPdfFile fileName $ \pdf -> do
        doc <- document pdf
        catalog <- documentCatalog doc
        rootNode <- catalogPageNode catalog
        cnt <- pageNodeNKids rootNode
        -- print cnt
        r <- forM [0 .. cnt - 1] $ \pageNum -> do
            -- r <- forM [1 .. 1] $ \pageNum -> do
            -- print $ "----- " ++ show pageNum ++ " -------------------------"
            page <- pageNodePageByNum rootNode pageNum
            (balances, trans) <- pageTransactionsCurrent page
            -- forM_ balances $ \op -> print op
            -- forM_ trans $ \op -> print op
            return (balances, trans)
        return r

-- ---------------------------------------------------------------------

readPdfCredit :: FilePath -> IO [([ByteString], [Transaction])]
readPdfCredit fileName = do
    withPdfFile fileName $ \pdf -> do
        doc <- document pdf
        catalog <- documentCatalog doc
        rootNode <- catalogPageNode catalog
        cnt <- pageNodeNKids rootNode
        print cnt
        r <- forM [0 .. cnt - 1] $ \pageNum -> do
            -- r <- forM [1 .. 1] $ \pageNum -> do
            -- print $ "----- " ++ show pageNum ++ " -------------------------"
            page <- pageNodePageByNum rootNode pageNum
            (balances, trans) <- pageTransactionsCredit page
            -- forM_ balances $ \op -> print op
            -- forM_ trans $ \op -> print op
            return (balances, trans)
        return r

-- ---------------------------------------------------------------------

pageTransactionsCurrent :: Page -> IO ([ByteString], [Transaction])
pageTransactionsCurrent page = do
    ops <- pageExtractOperators page
    -- print ("---ops----------------------------" :: String)
    -- forM_ ops $ \op -> print op
    -- print ("---textops----------------------------" :: String)
    let stuff = sort $ process $ textOpsOnly ops
    -- forM_ stuff $ \op -> print op
    -- print ("---grouped----------------------------" :: String)
    let grouped = groupByLine [] stuff
    -- forM_ grouped $ \op -> print op
    -- print ("---balances----------------------------" :: String)
    let (balances, body) = getBodyCurrent (False, Nothing, Nothing, []) grouped
    -- forM_ balances $ \op -> print op
    -- print ("---body----------------------------" :: String)
    -- forM_ body $ \op -> print op
    -- print ("---Tokens----------------------------" :: String)
    -- let tks = tokenise body
    -- forM_ tks $ \op -> print op
    -- print ("-------------------------------" :: String)
    let trans = analyzeBody body
    -- forM_ trans $ \op -> print op
    -- return (balancesBF, trans, balancesCF)
    return (balances, trans)

-- ---------------------------------------------------------------------

pageTransactionsCredit :: Page -> IO ([ByteString], [Transaction])
pageTransactionsCredit page = do
    ops <- pageExtractOperators page
    -- print ("---ops----------------------------" :: String)
    -- forM_ ops $ \op -> print op
    -- print ("---textops----------------------------" :: String)
    let stuff = sort $ process $ textOpsOnly ops
    -- forM_ stuff $ \op -> print op
    -- print ("---grouped----------------------------" :: String)
    let grouped = groupByLine [] stuff
    -- forM_ grouped $ \op -> print op
    -- print ("---balances----------------------------" :: String)
    let body = getBodyCredit grouped
    -- forM_ balances $ \op -> print op
    -- print ("---body----------------------------" :: String)
    -- forM_ body $ \op -> print op
    -- print ("---Tokens----------------------------" :: String)
    -- let tks = tokenise body
    -- forM_ tks $ \op -> print op
    -- print ("--transactions-----------------------------" :: String)
    let trans = reverse $ getTransactionsCredit [] body
    -- forM_ trans $ \op -> print op
    -- return (balancesBF, trans, balancesCF)
    return ([], trans)

-- ---------------------------------------------------------------------

getTransactionsCredit :: [Transaction] -> [(Scientific, [(Scientific, ByteString)])] -> [Transaction]
getTransactionsCredit acc [] = acc
getTransactionsCredit acc ((_, [(_, _d1), (_, _y1), (_, d2), (_, y2), (_, detail1), (_, detail2), (_, amount)]) : xs) =
    getTransactionsCredit
        ( Transaction
            { tType = CreditCardDebit
            , tDate = Just $ ccDate d2 y2
            , tDetail = Detail [toString detail1, toString detail2]
            , tAmount = Amount (toString amount)
            , tBalance = Nothing
            }
            : acc
        )
        xs
getTransactionsCredit acc ((_, [(_, _d1), (_, _y1), (_, d2), (_, y2), (_, detail), (_, amount), (_, "C"), (_, "R")]) : xs) =
    getTransactionsCredit
        ( Transaction
            { tType = CreditCardCredit
            , tDate = Just $ ccDate d2 y2
            , tDetail = Detail [toString detail]
            , tAmount = Amount (toString amount)
            , tBalance = Nothing
            }
            : acc
        )
        xs
getTransactionsCredit acc ((_, [(_, _d1), (_, _y1), (_, d2), (_, y2), (_, detail), (_, amount)]) : xs) =
    getTransactionsCredit
        ( Transaction
            { tType = CreditCardDebit
            , tDate = Just $ ccDate d2 y2
            , tDetail = Detail [toString detail]
            , tAmount = Amount (toString amount)
            , tBalance = Nothing
            }
            : acc
        )
        xs
getTransactionsCredit (t : acc) ((_, [(_, amt), (_, currency), (_, at), (_, rate)]) : xs) =
    getTransactionsCredit
        ( t
            { tDetail =
                addDetail
                    (toString amt ++ " " ++ toString currency ++ " " ++ toString at ++ " " ++ toString rate)
                    (tDetail t)
            }
            : acc
        )
        xs
getTransactionsCredit (t : acc) ((_, [(_, detail)]) : xs) =
    getTransactionsCredit
        ( t
            { tDetail =
                addDetail
                    (toString detail)
                    (tDetail t)
            }
            : acc
        )
        xs
getTransactionsCredit _acc (x : _xs) = error $ "getTransactionsCredit0:" ++ show x

-- ---------------------------------------------------------------------

ccDate :: ByteString -> ByteString -> Date
ccDate d y = Date (toString d ++ " " ++ toString y)

addDetail :: String -> Detail -> Detail
addDetail s (Detail ds) = Detail (ds ++ [s])

-- ---------------------------------------------------------------------

textOpsOnly :: [Operator] -> [Operator]
textOpsOnly ops = filter isWanted ops
  where
    isWanted :: Operator -> Bool
    isWanted (Op_Tm, _) = True
    isWanted (Op_Tj, _) = True
    isWanted _ = False

process :: [Operator] -> [(Scientific, Scientific, ByteString)]
process ((Op_Tm, [_, _, _, _, Number hor, Number ver]) : (Op_Tj, [String str]) : rest) =
    (ver, hor, str) : process rest
process (_ : xs) = process xs
process [] = []

groupByLine ::
    [(Scientific, [(Scientific, ByteString)])] ->
    [(Scientific, Scientific, ByteString)] ->
    [(Scientific, [(Scientific, ByteString)])]
groupByLine acc [] = acc
groupByLine [] [(ver, hor, txt)] = [(ver, [(hor, txt)])]
groupByLine [] ((ver2, hor2, txt2) : xs) = groupByLine [(ver2, [(hor2, txt2)])] xs
groupByLine ((ver1, as1) : as) ((ver2, hor2, txt2) : xs)
    | ver1 == ver2 = groupByLine ((ver1, (hor2, txt2) : as1) : as) xs
    | otherwise = groupByLine ((ver2, [(hor2, txt2)]) : (ver1, reverse as1) : as) xs

-- ---------------------------------------------------------------------

balanceAmount :: (Scientific, [(Scientific, ByteString)]) -> Maybe ByteString
balanceAmount (_, (_ : (_, "B") : (_, "A") : (_, "L") : (_, "A") : (_, "N") : (_, "C") : (_, "E") : bs)) =
    Just (snd $ last bs)
balanceAmount (_, ((_, "B") : (_, "A") : (_, "L") : (_, "A") : (_, "N") : (_, "C") : (_, "E") : bs)) =
    Just (snd $ last bs)
balanceAmount _ = Nothing

getBodyCurrent ::
    (Bool, Maybe ByteString, Maybe ByteString, [(Scientific, [(Scientific, ByteString)])]) ->
    [(Scientific, [(Scientific, ByteString)])] ->
    ([ByteString], [(Scientific, [(Scientific, ByteString)])])
getBodyCurrent (_, mo, mc, acc) [] = finishBody mo mc $ reverse acc
getBodyCurrent (_, mo, _, acc) [l] = finishBody mo (balanceAmount l) $ reverse acc
getBodyCurrent (False, mo, mc, []) (l : ls)
    | isJust (balanceAmount l) = getBodyCurrent (True, (balanceAmount l), mc, []) ls
    | otherwise = getBodyCurrent (False, mo, mc, []) ls
getBodyCurrent (False, _, _, _) _ = error ("should not happen" :: String)
getBodyCurrent (True, mo, _, acc) (l : ls)
    | isJust (balanceAmount l) = finishBody mo (balanceAmount l) $ reverse acc
    | otherwise = getBodyCurrent (True, mo, balanceAmount l, (l : acc)) ls

finishBody ::
    Maybe ByteString ->
    Maybe ByteString ->
    [(Scientific, [(Scientific, ByteString)])] ->
    ([ByteString], [(Scientific, [(Scientific, ByteString)])])
finishBody _ _ [] = ([], [])
finishBody mo mc ((_, [(_, ".")]) : bs) =
    (mapMaybe id [mo, mc], bs)
finishBody mo mc bs =
    (mapMaybe id [mo, mc], bs)

-- ---------------------------------------------------------------------

getBodyCredit ::
    [(Scientific, [(Scientific, ByteString)])] ->
    [(Scientific, [(Scientific, ByteString)])]
getBodyCredit bs = filter isBodyCredit bs

isBodyCredit :: (Scientific, [(Scientific, ByteString)]) -> Bool
isBodyCredit (_, [(_, d1), (_, _y1), (_, _d2), (_, _y2), (_, _detail), (_, _amount), (_, "C"), (_, "R")])
    | isJust (getDay (toDate d1)) = True
    | otherwise = False
isBodyCredit (_, [(_, d1), (_, _y1), (_, _d2), (_, _year2), (_, _detail1), (_, _detail2), (_, _amount)])
    | isJust (getDay (toDate d1)) = True
    | otherwise = False
isBodyCredit (_, [(_, d1), (_, _y1), (_, _d2), (_, _y2), (_, _detail), (_, _amount)])
    | isJust (getDay (toDate d1)) = True
    | otherwise = False
isBodyCredit (_, [(_, _), (_, _), (_, "@"), (_, _)]) = True
isBodyCredit (_, [(_, "MasterCard Exchange Rate")]) = True
isBodyCredit _ = False

toDate :: ByteString -> Date
toDate bs = Date (toString bs)

getDay :: Date -> Maybe Day
getDay (Date d) =
    parseTimeM False defaultTimeLocale "%d %b" d

-- ---------------------------------------------------------------------

data TransactionType
    = DirectDebit
    | BankPayment
    | OtherBankPayment
    | StopOrder
    | Visa
    | Atm
    | Credit
    | CreditCardDebit
    | CreditCardCredit
    deriving (Eq, Show)

data Date = Date String deriving (Eq, Show)
data Detail = Detail [String] deriving (Eq, Show)
data Amount = Amount String deriving (Eq, Show)
data Balance = Balance String deriving (Eq, Show)

data Token
    = TDate Date
    | TTransactionType TransactionType
    | TDetail Detail
    | TValue String
    | TNewLine
    deriving (Eq, Show)

data Transaction = Transaction
    { tType :: TransactionType
    , tDate :: Maybe Date
    , tDetail :: Detail
    , tAmount :: Amount
    , tBalance :: Maybe Balance
    }
    deriving (Eq, Show)

-- ---------------------------------------------------------------------

analyzeBody :: (HasCallStack) => [(Scientific, [(Scientific, ByteString)])] -> [Transaction]
analyzeBody ins = case parseTransactions (tokenise ins) of
    Right v -> v
    xx -> error $ show xx

debug :: a -> String -> a
debug a b = trace b a

-- ----------------------------------------------------------------------

type Parser = Parsec [ByteString] ()

parseTransactions :: [ByteString] -> Either ParseError [Transaction]
parseTransactions input = parse pTransactions "(unknown)" input

-- ----------------------------------------------------------------------

pTransactions :: (HasCallStack) => Parser [Transaction]
pTransactions = many pTransaction

pTransaction :: (HasCallStack) => Parser Transaction
pTransaction = do
    date <- optionMaybe pDate
    tt <- pTransactionType
    (detail, amt, bal) <- pDetailFinal (Detail [])
    return
        Transaction
            { tType = tt
            , tDate = date
            , tDetail = detail
            , tAmount = amt
            , tBalance = bal
            }

pDetailFinal :: Detail -> Parser (Detail, Amount, Maybe Balance)
pDetailFinal din = do
    detail <- pDetail
    nl <- optionMaybe pNewLine
    case nl of
        Just _ -> pDetailFinal (din <> detail)
        Nothing -> do
            amount <- pAmount
            total <- optionMaybe pBalance
            _ <- pNewLine
            return (din <> detail, amount, total)

pTransactionType :: Parser TransactionType
pTransactionType = do
    TTransactionType tt <- mytoken pbTransactionType
    return tt

pDate :: Parser Date
pDate = do
    TDate d <- mytoken pbDate
    return d

pDetail :: Parser Detail
pDetail = do
    TDetail d <- mytoken pbDetail
    return d

pAmount :: Parser Amount
pAmount = do
    TValue a <- mytoken pbValue
    return $ Amount a

pBalance :: Parser Balance
pBalance = do
    TValue a <- mytoken pbValue
    return $ Balance a

pNewLine :: Parser ()
pNewLine = do
    _ <- mytoken pbNewLine
    return ()

mytoken :: (HasCallStack) => PB.Parser a -> Parser a
mytoken parser = mysatisfy (parseToken parser)

-- ----------------------------------------------------------------------

tokenise :: (HasCallStack) => [(Scientific, [(Scientific, ByteString)])] -> [ByteString]
tokenise xs = concatMap doOne xs
  where
    doOne :: (Scientific, [(Scientific, ByteString)]) -> [ByteString]
    doOne (_, bss) = map snd bss ++ ["\n" :: ByteString]

-- Nested parsing. So ignore failure, as the enclosing parser will
-- deal with alternatives
parseToken :: (HasCallStack) => PB.Parser a -> ByteString -> Maybe a
parseToken parser bs =
    case pp of
        Right v -> Just v
        _ -> Nothing -- Fail, triggering next alternative
  where
    pp = parse parser "(unknown)" bs

pbNewLine :: (HasCallStack) => PB.Parser Token
pbNewLine = do
    _ <- char '\n'
    return TNewLine

pbDate :: (HasCallStack) => PB.Parser Token
pbDate = do
    d <- many1 digit
    _ <- char ' '
    m <- many1 letter
    _ <- char ' '
    y <- many1 digit
    return $ TDate (Date (d ++ " " ++ m ++ " " ++ y))

pbTransactionType :: (HasCallStack) => PB.Parser Token
pbTransactionType =
    (string "DD" >>= \_ -> return $ TTransactionType DirectDebit)
        <|> (string "BP" >>= \_ -> return $ TTransactionType BankPayment)
        <|> (string "OBP" >>= \_ -> return $ TTransactionType OtherBankPayment)
        <|> (string "SO" >>= \_ -> return $ TTransactionType StopOrder)
        <|> (string "VIS" >>= \_ -> return $ TTransactionType Visa)
        <|> (string "ATM" >>= \_ -> return $ TTransactionType Atm)
        <|> (string "CR" >>= \_ -> return $ TTransactionType Credit)

pbDetail :: (HasCallStack) => PB.Parser Token
pbDetail = do
    s <- many1 anyChar
    return $ TDetail (Detail [s])

pbValue :: (HasCallStack) => PB.Parser Token
pbValue = do
    v <- many1 (digit <|> char ',')
    _ <- char '.'
    v2 <- many1 digit
    return $ TValue (v ++ "." ++ v2)

-- ---------------------------------------------------------------------

mysatisfy ::
    (HasCallStack, Show t, Stream s m t) =>
    (t -> Maybe a) ->
    ParsecT s u m a
{-# INLINEABLE mysatisfy #-}
mysatisfy f =
    tokenPrim
        (\c -> show c)
        (\pos c _cs -> updatePosString pos (show c))
        (\c -> f c)

-- ---------------------------------------------------------------------

instance Semigroup Detail where
    (Detail a) <> (Detail b) = Detail (a <> b)
