import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Except

data LispError = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

showError:: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Excepted " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError action = catchError action $ return . show

extractValue::ThrowsError a -> a
extractValue (Right val) = val

symbol::Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr::String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

spaces::Parser ()
spaces = skipMany1 space

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool

parseString::Parser LispVal
parseString = do 
    _ <- char '"'
    x <- many $ noneOf "\""
    _ <- char '"'
    return $ String x

parseAtom::Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many $ letter <|> digit <|> symbol
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

parseNumber::Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr::Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuated
    <|> do 
            _ <-char '('
            x <- try parseList <|> parseDottedList
            _ <- char ')'
            return x

parseList::Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList::Parser LispVal
parseDottedList = do
    head_ <- endBy parseExpr spaces
    tail_ <- char '.' >> spaces >> parseExpr
    return $ DottedList head_ tail_

parseQuated::Parser LispVal
parseQuated = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

unwordsList::[LispVal] -> String
unwordsList = unwords . map showVal

showVal::LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head_ tail_) = "(" ++ unwordsList head_ ++ "." ++ showVal tail_ ++ ")"

instance Show LispVal where show = showVal

unpackNum::LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n ) = let parsed = reads n in
    if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

numericBinop::(Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

primitives::[(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
    ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)
    ]

apply::String ->[LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) $ lookup func primitives

eval::LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

main::IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
