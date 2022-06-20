import Data.List
import Data.Maybe
import qualified Data.Map as Map
import System.IO
import System.Environment

data Token = Num Integer
    | Keyword String
    | Id String
    | Symbol String
    deriving (Show)

{-
SymbolTable typeclass is synonym for Map of (String : Token)
-}
type SymbolTable = Map.Map [Char] Token

symbolTable :: SymbolTable
symbolTable = Map.fromList [
    ("return", Keyword "return"),
    ("int", Keyword "int"),
    (";", Symbol "semi-colon"),
    ("(", Symbol "left-bracket"),
    (")", Symbol "right-bracket"),
    ("{", Symbol "left-curl-bracket"),
    ("}", Symbol "right-curl-bracket"),
    ("&", Symbol "address"),
    ("*", Symbol "pointer")
    ]

stripWhiteSpace :: String -> [Char]
stripWhiteSpace [] = ""
stripWhiteSpace input = if firstChar == ' '
    then stripWhiteSpace (tail input)
    else firstChar : stripWhiteSpace (tail input)
    where firstChar = head input

-- tokenGen :: String -> Int -> [Token]
tokenGen :: String -> Int -> [Token]
tokenGen _ 0  = []
tokenGen [] _ = []
tokenGen line lenLine = case result of 
    --Just a -> a : tokenGen newLine newLen
    Just a -> [a] 
    Nothing -> []
    where   result = bruteLookup line lenLine
            newLine = drop 5 line 
            newLen = lenLine - 5

bruteLookup :: String -> Int -> Maybe Token
bruteLookup input ridx = if isNothing curSlice  
    then    bruteLookup input (ridx-1)
    else    curSlice
    where   curSlice = Map.lookup splice symbolTable
            splice = take (length input - ridx + 1) input

main :: IO()
main = do
    x   <- getArgs
    infile <- openFile (head x) ReadMode
    
    firstLine <- hGetLine infile 
    secondLine <- hGetLine infile
    thirdLine <- hGetLine infile

    let my_str = tokenGen firstLine (length firstLine)
    print my_str
    print firstLine
