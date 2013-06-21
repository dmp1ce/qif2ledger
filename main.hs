{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

--import Data.Typeable
import Data.List
import Data.List.Split
--import Data.Typeable
--import Data.String
--import Data.Function.Predicate

main :: IO ()
main = interact qifParse

qifParse :: String -> String
qifParse a = qifParseLines $ lines a

qifParseLines :: [String] -> String
qifParseLines [] = ""
qifParseLines a = do 
  let lineList = convertStringListToLineList a
  convertLineListToString lineList

convertStringListToLineList :: [String] -> [Line]
convertStringListToLineList [] = []
convertStringListToLineList a = map convertStringToLine a

convertStringToLine :: String -> Line
convertStringToLine (stripPrefix "!Type:" -> Just restOfString) = Type restOfString
convertStringToLine ('S':restOfString) = S restOfString
convertStringToLine ('^':[]) = End
convertStringToLine (stripPrefix "!Account" -> Just "") = Account
convertStringToLine ('N':restOfString) = N restOfString
convertStringToLine ('D':restOfString) = D restOfString
convertStringToLine ('T':restOfString) = T restOfString
convertStringToLine ('P':restOfString) = P restOfString
convertStringToLine ('M':restOfString) = M restOfString
convertStringToLine ('L':restOfString) = L restOfString
convertStringToLine ('E':restOfString) = E restOfString
convertStringToLine ('$':restOfString) = SubPrice $ '$':restOfString
convertStringToLine unknownString = Unknown unknownString

convertLineListToString :: [Line] -> String
-- Match Type and Account lines
convertLineListToString a = do
  let accountBlocks = splitOn [Account] a
  let accountTransactions = map convertAccountBlockToTransactions accountBlocks
  show accountTransactions
  --show a

type AccountBlock = [Line]
convertAccountBlockToTransactions :: AccountBlock -> [Transaction]
convertAccountBlockToTransactions a = do
  let transactionBlocks = splitOn [End] a
  let accountName = head a
  let accountType' = a !! 2
  let accountHeader = AccountHeader {name = lineToString accountName,
    accountType = lineToString accountType'
  } 
  let transactions = map (convertTransactionBlockToTransaction accountHeader) transactionBlocks 
  transactions

data AccountHeader = AccountHeader { name :: String,
  accountType :: String
}

showAccountHeaderName :: AccountHeader -> String
showAccountHeaderName (AccountHeader an _) = an

showAccountHeaderType :: AccountHeader -> String
showAccountHeaderType (AccountHeader _ at) = at

data Transaction = Transaction { date :: String
  , title :: String
  , account1 :: String
  , account2 :: String
  , amount  :: String
  } deriving (Show)

type TransactionBlock = [Line]
convertTransactionBlockToTransaction :: AccountHeader -> TransactionBlock -> Transaction
convertTransactionBlockToTransaction h a = do
  let maybeDate = find isTypeD a
  let maybePayee = find isTypeP a
  let maybeMessage = find isTypeM a
  let maybeAmount = find isTypeT a
  let maybeLocation = find isTypeL a
  -- Create the transaction from information collected
  Transaction {date = maybeLineToString maybeDate,
    title = maybeLineToString maybePayee ++ " - " ++ maybeLineToString maybeMessage,
    account1 = showAccountHeaderType h ++ ":" ++ showAccountHeaderName h,
    account2 = maybeLineToString maybeLocation,
    amount = maybeLineToString maybeAmount
    }

-- Helper functions to find types
isTypeD :: Line -> Bool
isTypeD (D _) = True
isTypeD (_) = False
isTypeP :: Line -> Bool
isTypeP (P _) = True
isTypeP (_) = False
isTypeT :: Line -> Bool
isTypeT (T _) = True
isTypeT (_) = False
isTypeM :: Line -> Bool
isTypeM (M _) = True
isTypeM (_) = False
isTypeL :: Line -> Bool
isTypeL (L _) = True
isTypeL (_) = False

-- Helper function to convert Line to string
maybeLineToString :: Maybe Line -> String
maybeLineToString (Just line) = lineToString line
maybeLineToString (Nothing) = ""

lineToString :: Line -> String
lineToString (D s) = s
lineToString (P s) = s
lineToString (T s) = s
lineToString (M s) = s
lineToString (L s) = s
lineToString (N s) = s
lineToString a = "Unknown Line type: " ++ show a


--splitLinesIntoAccountBlocks :: [Line] -> [AccountBlock]
--splitLinesIntoAccountBlocks (beforeAccount : Account : afterAccount) =
--  [beforeAccount] 
--splitLinesIntoAccountBlocks (Account : afterAccount) =
--  Account:afterAccount
--  afterAccount

--type AccountType = String
--convertNextBlockToString :: AccountType -> [Line] -> String
--convertNextBlockToString accountName lines = do
--  let date = "Hello"
--  date

data Line = Type String | S String | End | Account | N String |
  D String | T String | M String | P String | L String |
  E String | SubPrice String | Unknown String deriving (Show, Eq)
