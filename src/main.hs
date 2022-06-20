import Data.List
import Data.Maybe
import qualified Data.Map as Map
import System.IO
import System.Environment

data Token = Num Integer
    | Keyword String
    | Id String
    | Symbol String
    | Other String
    deriving (Show)

{-
SymjbolTable typeclass is synonym for Map of (String : Token)
-}
type SymbolTable = Map.Map [Char] Token

symbolTable :: SymbolTable
symbolTable = Map.fromList [
    ("return", Keyword "return"),
    ("int", Keyword "int"),
    ("char", Keyword "char"),
    (";", Symbol "semi-colon"),
    ("(", Symbol "left-bracket"),
    (")", Symbol "right-bracket"),
    ("{", Symbol "left-curl-bracket"),
    ("}", Symbol "right-curl-bracket"),
    ("&", Symbol "address"),
    ("*", Symbol "pointer"),
    ("\n", Other "new-line")
    ]

stripWhiteSpace :: String -> [Char]
stripWhiteSpace [] = ""
stripWhiteSpace input = if firstChar == ' '
    then stripWhiteSpace (tail input)
    else firstChar : stripWhiteSpace (tail input)
    where firstChar = head input

{-
    Matches for Symbols and Keywords
-}
tokenizeLine :: String -> [Token]
tokenizeLine [] = []
tokenizeLine input = tokenizeStatic input 1 ++ tokenizeLine (tail input)

tokenizeStatic :: String -> Int -> [Token]
tokenizeStatic input len = mapMaybe lookup combs --mapMaybe removes Maybes
    where
        tryPermutations :: String -> Int -> [String]
        tryPermutations input len = if len > length input
            then [""]
            else take len input : tryPermutations input (len+1)

        lookup :: String -> Maybe Token
        lookup input = Map.lookup input symbolTable

        combs = tryPermutations input len

main :: IO()
main = do
    x   <- getArgs
    infile <- openFile (head x) ReadMode 
    firstLine <- hGetContents infile

    let tokenList = tokenizeLine firstLine
    print tokenList

    print firstLine
