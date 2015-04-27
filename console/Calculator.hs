{-# LANGUAGE DeriveDataTypeable #-}

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Typeable
import System.IO

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Control.Monad.Reader as Reader

data Token =
  Number String Int |
  Identifier String Int |
  Operator Char Int |
  OpenParen Int |
  CloseParen Int
  deriving Show

data CalculatorException = CalculatorException { parseExMessage :: String } deriving (Show, Typeable)

instance Exception CalculatorException

data BinaryOperator = Assign | Plus | Minus | Multiply | Divide | Power deriving (Eq, Show)

data BinaryOperatorInfo a = BinaryOperatorInfo {
  binOp :: BinaryOperator,
  binOpChar :: Char,
  binOpPrecedence :: Int,
  binOpFunction :: String -> Int -> a -> a -> a }

data AST a =
  ASTNumber a |
  ASTIdentifier String Int |
  ASTBinaryOperator BinaryOperator (AST a) (AST a) Int |
  ASTFunction String (AST a) Int
  deriving Show

divide expr loc l r | (r == 0.0) = throwError CalculatorException "Evaluation error. Division by zero" expr loc
divide _ _ l r = l / r

const2 = const . const

binaryOperators :: (Eq a, Floating a) => [BinaryOperatorInfo a]
binaryOperators = [
  BinaryOperatorInfo Assign '=' 0 undefined,
  BinaryOperatorInfo Plus '+' 1 (const2 (+)),
  BinaryOperatorInfo Minus '-' 1 (const2 (-)),
  BinaryOperatorInfo Multiply '*' 2 (const2 (*)),
  BinaryOperatorInfo Divide '/' 2 divide,
  BinaryOperatorInfo Power '^' 3 (const2 (**))]

operators = map binOpChar binaryOperators

minPrecedence = minimum (map binOpPrecedence binaryOperators)
findBinOpInfoByChar op | Just info <- List.find ((op==) . binOpChar) binaryOperators = info
findBinOpInfoByOp op | Just info <- List.find ((op==) . binOp) binaryOperators = info

functions :: (Floating a) => [(String, a -> a)]
functions = [("exp", exp), ("sqrt", sqrt), ("log", log), ("sin", sin), ("tan", tan), ("cos", cos),
  ("asin", asin), ("atan", atan), ("acos", acos), ("sinh", sinh), ("tanh", tanh), ("cosh", cosh),
  ("asinh", asinh), ("atanh", atanh), ("acosh", acosh), ("abs", abs)]

constants :: (Floating a) => [(String, a)]
constants = [("pi", pi), ("e", exp 1)]

throwError :: (String -> CalculatorException) -> String -> String -> Int -> a
throwError ex msg expr at = throw $ ex $ msg ++ ":\n" ++ expr ++ "\n" ++ replicate at ' ' ++ "^"

-- Lexer

tokenLoc :: Token -> Int
tokenLoc (Number _ loc) = loc
tokenLoc (Identifier _ loc) = loc
tokenLoc (Operator _ loc) = loc
tokenLoc (OpenParen loc) = loc
tokenLoc (CloseParen loc) = loc

tokenLength :: Token -> Int
tokenLength (Number n _) = length n
tokenLength (Identifier i _) = length i
tokenLength _ = 1

scanToken :: String -> Int -> String -> Maybe Token
scanToken _ loc ('(':_) = Just $ OpenParen loc
scanToken _ loc (')':_) = Just $ CloseParen loc
scanToken _ loc (c:_) | isOperator c = Just $ Operator c loc
scanToken expr loc s | isDigit (head s) = Just $ Number n loc where
  intPart = takeWhile isDigit s
  n = case drop (length intPart) s of
    ('.':d:tail) | isDigit d -> intPart ++ "." ++ [d] ++ takeWhile isDigit tail
    ('.':d:_) -> throwError CalculatorException "Lexical error. Unexpected symbol" expr (loc + length intPart + 1)
    ('.':_) -> throwError CalculatorException "Lexical error. Unexpected end of input" expr (loc + length intPart + 1)
    _ -> intPart
scanToken _ loc s | isLetter (head s) = Just $ Identifier (takeWhile isLetter s) loc
scanToken _ _ _ = Nothing

tokenize :: String -> [Token]
tokenize expr = length tokens `seq` tokens where
  tokens = scan 0 expr
  scan _ [] = []
  scan loc (' ':tail) = scan (loc+1) tail
  scan loc s = maybe
    (throwError CalculatorException "Lexical error. Unexpected symbol:" expr loc)
    (\token -> let n = tokenLength token in token : scan (loc+n) (drop n s))
    (scanToken expr loc s)

isDigit n = n >= '0' && n <= '9'
isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
isOperator c = elem c operators

-- Parser

parsePrimary :: (Floating a, Read a) => String -> [Token] -> (AST a, [Token])
parsePrimary expr [] = throwError CalculatorException "Syntax error. Unexpected end of input" expr (length expr)
parsePrimary _ (Number n _ : tail) = (ASTNumber $ read n, tail)
parsePrimary expr (OpenParen _ : tokens) = parseParens expr tokens
parsePrimary expr (Identifier i loc : tokens) = case tokens of
  (OpenParen _ : tail) -> let (arg, tail') = parseParens expr tail in (ASTFunction i arg loc, tail')
  _ -> (ASTIdentifier i loc, tokens)
parsePrimary expr (t:_) = throwError CalculatorException "Unexpected token" expr (tokenLoc t)

parseParens :: (Floating a, Read a) => String -> [Token] -> (AST a, [Token])
parseParens expr tokens = case parseExpression expr tokens of
  (ast, CloseParen _ : tail) -> (ast, tail)
  (_, t:_) -> throwError CalculatorException "Syntax error. Unexpected token" expr (tokenLoc t)
  _ -> throwError CalculatorException "Syntax error. Expected )" expr (length expr)

-- Algorithm from https://en.wikipedia.org/wiki/Operator-precedence_parser
parseExpression :: (Floating a, Read a) => String -> [Token] -> (AST a, [Token])
parseExpression expr tokens | (ast, tail) <- parsePrimary expr tokens = parseExpression1 expr ast minPrecedence tail

innerLoop :: (Floating a, Read a) => String -> AST a -> Int -> [Token] -> (AST a, [Token])
innerLoop expr rhs opPrecedence tokens = case tokens of
  (Operator op _ : _) |
    precedence <- binOpPrecedence (findBinOpInfoByChar op),
    precedence > opPrecedence,
    (rhs', tail) <- parseExpression1 expr rhs precedence tokens ->
      innerLoop expr rhs' opPrecedence tail
  _ -> (rhs, tokens)

parseExpression1 :: (Floating a, Read a) => String -> AST a -> Int -> [Token] -> (AST a, [Token])
parseExpression1 expr lhs minPrecedence tokens = case tokens of
  (Operator op loc : tail) |
    opInfo <- findBinOpInfoByChar op,
    binOpPrecedence opInfo >= minPrecedence,
    (rhs', tail') <- parsePrimary expr tail,
    (rhs'', tail'') <- innerLoop expr rhs' (binOpPrecedence opInfo) tail' ->
      parseExpression1 expr (ASTBinaryOperator (binOp opInfo) lhs rhs'' loc) minPrecedence tail''
  _ -> (lhs, tokens)

parse :: (Floating a, Show a, Read a) => String -> [Token] -> AST a
parse expr tokens | (ast, tail) <- parseExpression expr tokens = case tail of
  (t:_) -> throwError CalculatorException "Syntax error. Unexpected token" expr (tokenLoc t)
  _ -> ast

-- Interpreter

type VarTable = Map.Map String
type Eval a = Reader.Reader (String, VarTable a)

calc :: (Eq a, Floating a) => AST a -> Eval a a
calc (ASTNumber a) = return a
calc (ASTBinaryOperator op l r loc) = do
  (expr, _) <- Reader.ask
  if op == Assign
    then throwError CalculatorException "Evaluation error. Assignment operator cannot be used in expressions" expr loc
    else do
      left <- calc l
      right <- calc r
      return $ (binOpFunction $ findBinOpInfoByOp op) expr loc left right
calc (ASTIdentifier i loc) = do
  (expr, varTable) <- Reader.ask
  return $ case Map.lookup i varTable of
    Nothing -> throwError CalculatorException ("Evaluation error. Variable '" ++ i ++ "' is undefined") expr loc
    Just val -> val
calc (ASTFunction i arg loc) = do
  (expr, _) <- Reader.ask
  case lookup i functions of
    Nothing -> throwError CalculatorException ("Evaluation error. Function '" ++ i ++ "' is undefined") expr loc
    Just func -> func `liftM` calc arg

evalAndUpdateTable ast xs expr varTable |
  value <- Reader.runReader (calc ast) (expr, varTable) = (foldr (\i m -> Map.insert i value m) varTable xs, value)

eval :: (Eq a, Floating a) => AST a -> String -> VarTable a -> (VarTable a, a)
eval (ASTBinaryOperator Assign (ASTIdentifier i _) ast _) = evalAndUpdateTable ast ["it", i]
eval ast = evalAndUpdateTable ast ["it"]

readInput :: (Eq a, Floating a, Read a, Show a) => VarTable a -> IO (String, VarTable a, Bool)
readInput varTable = do
  putStr "> "
  hFlush stdout
  str <- getLine
  if null str then return ("", varTable, True) else catch
    (evaluate $ case parse str (tokenize str) of
      ASTIdentifier "quit" _ -> ("Bye-bye!", varTable, False)
      ast -> let (modifiedVarTable, result) = eval ast str varTable in
        result `seq` (show result, modifiedVarTable, True))
    (\e -> return (parseExMessage e, varTable, True))

repl :: (Eq a, Floating a, Read a, Show a) => VarTable a -> IO ()
repl varTable = do
  (output, modifiedVarTable, repeat) <- readInput varTable
  when (not $ null output) $ do
    putStrLn output
    putStrLn ""
  when (repeat) (repl modifiedVarTable)

main = do
  putStrLn "Calculator (enter 'quit' to quit)"
  repl (Map.fromList constants :: VarTable Double)
