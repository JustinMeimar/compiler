import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Map as Map
import System.IO
import System.Environment

data Token = Num Integer
    | Keyword String
    | Id String
    | Symbol String
    | Other String
    deriving (Show, Eq)

{-typeclass is synonym for Map of (String : Token)-}
type SymbolTable = Map.Map [Char] Token
type KeywordTable = Map.Map [Char] Token

symbolTable :: SymbolTable
symbolTable = Map.fromList [   
    (";", Symbol "semi-colon"),
    ("(", Symbol "left-bracket"),
    (")", Symbol "right-bracket"),
    ("{", Symbol "left-curl-bracket"),
    ("}", Symbol "right-curl-bracket"),
    ("&", Symbol "address"),
    ("*", Symbol "pointer"),
    ("\n", Other "new-line")
    ]

keywordTable :: KeywordTable
keywordTable = Map.fromList [
    ("return ", Keyword "return"),
    ("int", Keyword "int"),
    ("char", Keyword "char")
    ]

stripWhiteSpace :: String -> [Char]
stripWhiteSpace [] = ""
stripWhiteSpace input = if firstChar == ' '
    then stripWhiteSpace (tail input)
    else firstChar : stripWhiteSpace (tail input)
    where firstChar = head input


-- Check for C keywords, ex: return, int, char, for .. etc 
isKeywordTok :: String -> Bool
isKeywordTok input = isJust (Map.lookup input keywordTable)

-- Check for symbols in symboLTable, ex: *, &, +, -,  
isSymbolTok :: String -> Bool
isSymbolTok  input = isJust (Map.lookup input symbolTable)

-- Check for constant Numbers ex x = 5;
isNumberTok :: String -> Bool
isNumberTok input = (&&) (not False `elem` map isNumber input) (not (isNumber (last input)))

-- Check for Variables and Function Names
isIdTok :: String -> Bool
isIdTok input = (&&) (not False `elem` map isValidChar input) (not (isValidChar (last input)))
    where   
        isValidChar :: Char -> Bool
        isValidChar char = (||) (isAlphaNum char) (char == '_')

itterativeMatch :: String -> Int -> (Int, Token)
itterativeMatch [] _ = (0, Keyword "End")
itterativeMatch input 10 = (0, Keyword "None")  -- No Tokens over 10 chars long? 
itterativeMatch input len = case () of _
                                        | isKeywordTok curSlice -> (lenTok, Keyword curSlice)
                                        | isSymbolTok curSlice -> (lenTok, Symbol curSlice)
                                        | isNumberTok curSlice -> (lenTok-1, Id (init curSlice))
                                        | isIdTok curSlice -> (lenTok-1, Id (init curSlice)) 
                                        | otherwise -> itterativeMatch input (len+1)
    where
        curSlice = take len input
        lenTok = length curSlice

getNextTokenList :: String -> [Token]
getNextTokenList [] = [Keyword "End"]
getNextTokenList program = token : getNextTokenList newProgram
    where   tokenProgramTuple = getNextToken program
            token = snd tokenProgramTuple 
            newProgram = fst tokenProgramTuple

getNextToken :: String -> (String, Token)
getNextToken program = if snd token == Keyword "None"
    then getNextToken (drop 1 program)
    else (newProgram, snd token)
    where
        token = itterativeMatch program 1
        newProgram = drop (fst token) program
       
