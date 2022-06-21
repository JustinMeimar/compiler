-- TODO: Import lexer module
       
main :: IO()
main = do
    x   <- getArgs
    infile <- openFile (head x) ReadMode 
    program <- hGetContents infile

    let tokenList = getNextTokenList program
    print tokenList
    print program

    return()