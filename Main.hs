{-# LANGUAGE ExistentialQuantification, NamedFieldPuns #-}

module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Applicative ((<$>))
import Control.Monad.Error
import Data.Maybe
import System.IO
import Data.IORef

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = do
        result <- runErrorT (trapError action)
        return $ extractValue result

isBound :: Env -> String -> IO Bool
isBound envRef var = do
        env <- readIORef envRef
        return . isJust $ lookup var env

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
        env <- liftIO $ readIORef envRef
        maybe (throwError $ UnboundVar "Unbound variable" var)
              (liftIO . readIORef)
              (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
        env <- liftIO $ readIORef envRef
        maybe (throwError $ UnboundVar "Unbound variable" var)
              (liftIO . (`writeIORef` value))
              (lookup var env)
        return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
        alreadyDefined <- liftIO $ isBound envRef var
        if alreadyDefined
            then setVar envRef var value >> return value
            else liftIO $ do
                valueRef <- newIORef value
                env <- readIORef envRef
                writeIORef envRef ((var, valueRef) : env)
                return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) $ mapM addBinding bindings
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String]
                    , varargs :: Maybe String
                    , body :: [LispVal]
                    , env :: Env
                    }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varName) = message ++ ": " ++ varName
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message f) = message ++ ": " ++ show f
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseError) = "Parse error at " ++ show parseError

instance Show LispError where show = showError

instance Error LispError where
        noMsg = Default "An error has occurred"
        strMsg = Default

type ThrowsError = Either LispError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input =
        case parse parser "lisp" input of
            Left err -> throwError $ Parser err
            Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr
readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (parseExpr `endBy` spaces)

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params, varargs, body, env}) =
        "(lambda (" ++ unwords (map show params) ++
            (case varargs of
                Nothing -> ""
                Just arg -> " . " ++ arg) ++ ") ...)"

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
            do result <- eval env pred
               case result of
                   Bool True  -> eval env conseq
                   Bool False -> eval env alt
                   notBool    -> throwError $ TypeMismatch "bool" notBool
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List [Atom "load", String fileName]) = do
        program <- load fileName
        last <$> mapM (eval env) program
eval env (List (Atom "define" : List (Atom var : params) : body)) =
        makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
        makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
        makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
        makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgs varargs env [] body
eval env (List (func : args)) = do
        f <- eval env func
        argVals <- mapM (eval env) args
        apply f argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc f) args = liftThrows $ f args
apply (Func {params, varargs, body, env}) args =
    if num params /= num args && isNothing varargs
        then throwError $ NumArgs (num params) args
        else (liftIO . bindVars env $ zip params args)
             >>= bindVarArgs varargs
             >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = last <$> mapM (eval env) body
          bindVarArgs arg env =
              case arg of
                  Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
                  Nothing -> return env
apply (IOFunc func) args = func args
apply notAFunc _ = throwError $ TypeMismatch "func" notAFunc

primitiveBindings :: IO Env
primitiveBindings = do
        n <- nullEnv 
        bindVars n $ map (makeFunc IOFunc) ioPrimitives
                  ++ map (makeFunc PrimitiveFunc) primitives
    where makeFunc constructor (var, f) = (var, constructor f)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numericBinOp (+))
             , ("-", numericBinOp (-))
             , ("*", numericBinOp (*))
             , ("/", numericBinOp div)
             , ("mod", numericBinOp mod)
             , ("quotient", numericBinOp quot)
             , ("remainder", numericBinOp rem)
             , ("=", numBoolBinOp (==))
             , ("<", numBoolBinOp (<))
             , (">", numBoolBinOp (>))
             , ("/=", numBoolBinOp (/=))
             , (">=", numBoolBinOp (>=))
             , ("<=", numBoolBinOp (<=))
             , ("&&", boolBoolBinOp (&&))
             , ("||", boolBoolBinOp (||))
             , ("string=?", strBoolBinOp (==))
             , ("string<?", strBoolBinOp (<))
             , ("string>?", strBoolBinOp (>))
             , ("string<=?", strBoolBinOp (<=))
             , ("string>=?", strBoolBinOp (>=))
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             , ("eq?", eqv)
             , ("eqv?", eqv)
             , ("equal?", equal)
             ]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [ ("apply", applyProc)
               , ("open-input-file", makePort ReadMode)
               , ("open-output-file", makePort WriteMode)
               , ("close-input-port", closePort)
               , ("close-output-port", closePort)
               , ("read", readProc)
               , ("write", writeProc)
               , ("read-contents", readContents)
               , ("read-all", readAll)
               ] 

boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp unpacker op args =
        if length args /= 2
            then throwError $ NumArgs 2 args
            else do left <- unpacker $ head args
                    right <- unpacker $ args !! 1
                    return . Bool $ left `op` right

numBoolBinOp = boolBinOp unpackNum
strBoolBinOp = boolBinOp unpackStr
boolBoolBinOp = boolBinOp unpackBool

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp _ [] = throwError $ NumArgs 2 []
numericBinOp _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinOp op params = liftM (Number . foldl1 op) $ mapM unpackNum params

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x : xs)
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x, y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool a, Bool b] = return . Bool $ a == b
eqv [Number a, Number b] = return . Bool $ a == b
eqv [String a, String b] = return . Bool $ a == b
eqv [Atom a, Atom b] = return . Bool $ a == b
eqv [DottedList xs x, DottedList ys y] = eqv [List (xs ++ [x]), List (ys ++ [y])]
eqv [List xs, List ys] = return . Bool $ length xs == length ys &&
                                         all eqvPair (xs `zip` ys)
    where eqvPair (x, y) = case eqv [x, y] of
                               Left _ -> False
                               Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals a b (AnyUnpacker unpacker) =
        do ua <- unpacker a
           ub <- unpacker b
           return $ ua == ub
        `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [a, b] = do
        primitiveEquals <- liftM or $ mapM (unpackEquals a b)
                                           [ AnyUnpacker unpackNum
                                           , AnyUnpacker unpackStr
                                           , AnyUnpacker unpackBool
                                           ]
        Bool eqvEquals <- eqv [a, b]
        return . Bool $ (primitiveEquals || eqvEquals)
equal badArgList = throwError $ NumArgs 2 badArgList

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [f, List args] = apply f args
applyProc (f : args)     = apply f args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String fileName] = liftM Port . liftIO $ openFile fileName mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = do
        liftIO $ hClose port 
        return $ Bool True
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = do
        line <- liftIO $ hGetLine port
        liftThrows $ readExpr line

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = do
        liftIO $ hPrint port obj
        return $ Bool True

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String fileName] = liftM String $ liftIO $ readFile fileName

load :: String -> IOThrowsError [LispVal]
load fileName = do
        contents <- liftIO $ readFile fileName
        liftThrows $ readExprList contents

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String fileName] = List <$> load fileName

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows . liftM show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = do
        result <- evalString env expr
        putStrLn result

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
        result <- prompt
        unless (pred result) $ do
            action result
            until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do
        env <- do
            prims <- primitiveBindings
            bindVars prims [("args", List . map String $ tail args)]
        result <- runIOThrows $ liftM show $ eval env (List [Atom "load", String (head args)])
        hPutStrLn stderr result

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp» ") . evalAndPrint

main :: IO ()
main = do
        args <- getArgs
        if null args
            then runRepl
            else runOne args
