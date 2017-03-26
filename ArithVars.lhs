Model solution for Module 08: Variables, CSCI 360 @ Hendrix College
4 October 2016
Brent Yorgey

> {-# LANGUAGE GADTs #-}
>
> module ArithVars where
>
> import qualified Data.Map as M
> import           Parsing  hiding ((<$), (<$>), (<*>))
> import           Prelude
>
> -- Abstract syntax
>
> data Arith where
>   Lit :: Integer -> Arith
>   Bin :: Op -> Arith -> Arith -> Arith
>   Var :: String -> Arith
>   Let :: String -> Arith -> Arith -> Arith
>   deriving (Show)
>
> data Op where
>   Plus  :: Op
>   Minus :: Op
>   Times :: Op
>   Div   :: Op
>   deriving (Show, Eq)
>
> -- Parser
>
> lexer :: TokenParser u
> lexer = makeTokenParser $ emptyDef
>   { reservedNames = ["let", "in"] }
>
> parens :: Parser a -> Parser a
> parens     = getParens lexer
>
> reservedOp :: String -> Parser ()
> reservedOp = getReservedOp lexer
>
> reserved :: String -> Parser ()
> reserved   = getReserved lexer
>
> integer :: Parser Integer
> integer    = getInteger lexer
>
> whiteSpace :: Parser ()
> whiteSpace = getWhiteSpace lexer
>
> identifier :: Parser String
> identifier = getIdentifier lexer
>
> parseArithAtom :: Parser Arith
> parseArithAtom =
>       Lit <$> integer
>   <|> Var <$> identifier
>   <|> parseLet
>   <|> parens parseArith
>
> parseLet :: Parser Arith
> parseLet = Let
>   <$> (reserved   "let" *> identifier)
>   <*> (reservedOp "="   *> parseArith)
>   <*> (reserved   "in"  *> parseArith)
>
> parseArith :: Parser Arith
> parseArith = buildExpressionParser table parseArithAtom
>   where
>     table = [ [ Infix (Bin Times <$ reservedOp "*") AssocLeft
>               , Infix (Bin Div   <$ reservedOp "/") AssocLeft
>               ]
>             , [ Infix (Bin Plus  <$ reservedOp "+") AssocLeft
>               , Infix (Bin Minus <$ reservedOp "-") AssocLeft
>               ]
>             ]
>
> arith :: Parser Arith
> arith = whiteSpace *> parseArith <* eof
>
> -- Interpreter
>
> type Env = M.Map String Integer
>
> data InterpError where
>   UnboundVar :: String -> InterpError
>   DivByZero  :: InterpError
>
> showInterpError :: InterpError -> String
> showInterpError (UnboundVar x) = "Unbound variable " ++ x
> showInterpError DivByZero      = "Division by zero"
>
> interpArith :: Env -> Arith -> Either InterpError Integer
> interpArith _ (Lit i)           = Right i
> interpArith e (Bin Plus e1 e2)  = (+) <$> interpArith e e1 <*> interpArith e e2
> interpArith e (Bin Minus e1 e2) = (-) <$> interpArith e e1 <*> interpArith e e2
> interpArith e (Bin Times e1 e2) = (*) <$> interpArith e e1 <*> interpArith e e2
> interpArith e (Bin Div e1 e2)   =
>   interpArith e e2 >>= \v ->
>   case v of
>     0 -> Left DivByZero
>     _ -> div <$> interpArith e e1 <*> Right v
> interpArith e (Var x)           =
>   case M.lookup x e of
>     Nothing -> Left $ UnboundVar x
>     Just v  -> Right v
> interpArith e (Let x e1 e2)     =
>   interpArith e e1 >>= \v ->
>   interpArith (M.insert x v e) e2
>
> -- Evaluator
>
> eval :: String -> IO ()
> eval s = case parse arith s of
>   Left pErr  -> print pErr
>   Right e    ->
>     case interpArith M.empty e of
>       Left iErr -> putStrLn (showInterpError iErr)
>       Right v   -> print v
