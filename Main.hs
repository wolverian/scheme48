module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Applicative ((<$>))

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where show = showVal

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

stringChar :: Parser String
stringChar = notQuote <|> escapedQuote
        where

        notQuote = do
            c <- noneOf "\"" 
            return [c]

        escapedQuote = do
            q <- char '\\' 
            c <- char '"'
            return $ q:[c]

parseString :: Parser LispVal
parseString = do
        char '"'
        x <- many (noneOf "\"")
        char '"'
        return $ String x

parseAtom :: Parser LispVal
parseAtom = do
        first <- letter <|> symbol
        rest <- many (letter <|> digit <|> symbol)
        let atom = first : rest
        return $ case atom of
                     "#t" -> Bool True
                     "#f" -> Bool False
                     _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> parseExpr `sepBy` spaces

parseDottedList :: Parser LispVal
parseDottedList = do
        h <- parseExpr `endBy` spaces
        t <- char '.' >> spaces >> parseExpr
        return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
        char '\''
        x <- parseExpr
        return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> between (char '(') (char ')') (try parseList <|> parseDottedList)

readExpr :: String -> LispVal
readExpr input =
        case parse parseExpr "lisp" input of
            Left err -> String $ "No match: " ++ show err
            Right val -> val

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom f : args)) = apply f $ map eval args

apply :: String -> [LispVal] -> LispVal
apply f args = maybe (Bool False) ($ args) $ lookup f primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [ ("+", numericBinOp (+))
             , ("-", numericBinOp (-))
             , ("*", numericBinOp (*))
             , ("/", numericBinOp div)
             , ("mod", numericBinOp mod)
             , ("quotient", numericBinOp quot)
             , ("remainder", numericBinOp rem)
             ]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op params = Number . foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

main :: IO ()
main = do
        args <- getArgs
        print . eval . readExpr $ head args
