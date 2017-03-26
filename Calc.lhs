Project 2: Calculator
=====================

For this project, you will implement the guts of a (fancy) calculator.
I have provided you with a simple read-eval-print interface (in
[CalcREPL.hs](CalcREPL.hs)) where the user can type in expressions to
be evaluated.  You will provide a function of type `String -> String`
which accepts the user's input and produces a response.  Of course,
your `String -> String` function should be decomposed into multiple
phases, just like all of the language implementations we have been
considering (such as parsing, pretty-printing, interpreting, and so
on).

- Simply recompile and re-run `CalcREPL` each time you want to test
  changes you have made to `Calc.lhs`.


Starter code
------------

> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE GADTs #-}
>
> module Calc where
> import Parsing
> import Data.Maybe
> import Prelude hiding ((<$>), (<$), (<*>))

> helpMsg :: String
> helpMsg = unlines
>   [ " You can use integers or floating point values,"
>   , "negation, or standard arithmetic operators + - * / ^ ."
>    , "You can also use functions like sine (sin), cosine (cos), log, floor, ceiling, abs"
>    , "use them as follow: sin(), cos(), log(), floor(), ceiling(), abs()"
>    , "you can also use constants like pi and e"
>    , "sin(2pi / 4) + 6 is a valid expresion"
>    ,"also make sure to put space before and after standard arithmetic operators."
>    , "1 + -3 is a valid expression, thus, negative numbers are allowed."
>   ]
>
> ---data tree
> data Arith  where
>   Lit :: Double -> Arith
>   Pi :: Arith
>   E ::  Arith
>   Add :: Arith -> Arith -> Arith
>   Sub :: Arith -> Arith -> Arith
>   Mul :: Arith -> Arith -> Arith
>   Div :: Arith -> Arith -> Arith
>   Exp :: Arith -> Arith -> Arith
>   Neg :: Arith -> Arith
>   Sin :: Arith -> Arith
>   Cos :: Arith -> Arith
>   Tan :: Arith -> Arith
>   Log :: Arith -> Arith
>   Abs :: Arith -> Arith
>   Floor :: Arith -> Arith
>   Ceiling :: Arith -> Arith
>   Sqrt :: Arith -> Arith
>   deriving (Show)
>
> -- interpretror
> interpArith :: Arith -> Double
> interpArith (Lit i) = i
> interpArith Pi = pi
> interpArith E = exp 1
> interpArith (Add e1 e2) = interpArith e1 + interpArith e2
> interpArith (Sub e1 e2) = interpArith e1 - interpArith e2
> interpArith (Mul e1 e2) = interpArith e1 * interpArith e2
> interpArith (Div e1 e2) = interpArith e1 / interpArith e2
> interpArith (Exp e1 e2) = interpArith e1 ** interpArith e2
> interpArith (Neg i) = - (interpArith i)
> interpArith (Sin e1) = sin (interpArith e1)
> interpArith (Cos e1) = cos (interpArith e1)
> interpArith (Tan e1) = tan (interpArith e1)
> interpArith (Log e1) = log (interpArith e1)
> interpArith (Abs e1) = abs (interpArith e1)
> interpArith (Floor e1) = fromIntegral (floor(interpArith e1)) :: Double
> interpArith (Ceiling e1) = fromIntegral (ceiling (interpArith e1)) :: Double
> interpArith (Sqrt e1) = sqrt (interpArith e1)
>
> lexer :: TokenParser u
> lexer = makeTokenParser emptyDef
>
> parens :: Parser a -> Parser a
> parens     = getParens lexer
>
> reservedOp :: String -> Parser ()
> reservedOp = getReservedOp lexer
>
> integer :: Parser Integer
> integer    = getInteger lexer
>
> symbol :: Parser String
> symbol = getSymbol lexer ""
>
> naturalOrFloat :: Parser (Either Integer Double)
> naturalOrFloat = getNaturalOrFloat lexer
>
> whiteSpace :: Parser ()
> whiteSpace = getWhiteSpace lexer
>
>
> parseArithHelper :: Either Integer Double -> Arith
> parseArithHelper (Left a) = Lit (fromIntegral a :: Double)
> parseArithHelper (Right b) = Lit b
>
> -- more parsers
> parseArithAtom :: Parser Arith
> parseArithAtom = Sin <$> (reservedOp "sin" *> parens parseArith)
>                  <|> Cos <$> (reservedOp "cos" *> parens parseArith)
>                  <|> Tan <$> (reservedOp "tan" *> parens parseArith)
>                  <|> Log <$> (reservedOp "log" *> parens parseArith)
>                  <|> Abs <$> (reservedOp "abs" *> parens parseArith)
>                  <|> Floor <$> (reservedOp "floor" *> parens parseArith)
>                  <|> Ceiling <$> (reservedOp "ceiling" *> parens parseArith)
>                  <|> Sqrt <$> (reservedOp "sqrt" *> parens parseArith)
>                  <|> Pi <$ reservedOp "pi"
>                  <|> E <$ reservedOp "e"
>                  <|> parseArithHelper  <$> naturalOrFloat
>                  <|> parens parseArith
>
>
> parseArith :: Parser Arith
> parseArith = buildExpressionParser table parseArithAtom
>   where
>     table = [[ Infix (Exp <$ reservedOp "^") AssocRight],
>             [ Infix (Mul <$ reservedOp "*") AssocLeft, Infix (Div <$ reservedOp "/") AssocLeft ]
>             ,[ Prefix (Neg <$ reservedOp "-"), Infix (Add <$ reservedOp "+") AssocLeft
>               , Infix (Sub <$ reservedOp "-") AssocLeft
>               ]
>             ]
> -- evaluate
> eval :: String -> Maybe Double
> eval  s = case parseSome parseArith s of
>    Right (v, _) -> Just (interpArith v)
>    _            -> Nothing
>
>  -- pretty-print
> pprintHelper :: String -> Maybe Arith
> pprintHelper s = case parseSome parseArith s of
>   Right (b, _) -> Just b
>   _            -> Nothing
>
> pprint' :: Arith -> String
> pprint' (Lit i) = show i
> pprint' Pi =  "\960"
> pprint' E = show "e"
> pprint' (Add e1 e2) = "(" ++ pprint' e1 ++ " + " ++ pprint' e2 ++ ")"
> pprint' (Sub e1 e2) = "(" ++ pprint' e1 ++ " - " ++ pprint' e2 ++ ")"
> pprint' (Mul e1 e2) = pprint' e1 ++ " * "++ pprint' e2
> pprint' (Exp e1 e2) = pprint' e1 ++ " ^ " ++ pprint' e2
> pprint' (Neg i) = "-" ++ pprint' i
> pprint' (Sin i) = "sin(" ++ pprint' i ++ ")"
> pprint' (Cos i) = "cos(" ++ pprint' i ++ ")"
> pprint' (Tan i) = "tan(" ++ pprint' i ++ ")"
> pprint' (Log i) = "log(" ++ pprint' i ++ ")"
> pprint' (Abs i) = "abs(" ++ pprint' i ++ ")"
> pprint' (Floor i) = "floor(" ++ pprint' i ++ ")"
> pprint' (Ceiling i) = "ceiling(" ++ pprint' i ++ ")"
> pprint' (Sqrt i) = "sqrt(" ++ pprint' i ++ ")"
> pprint' (Div e1 e2) = pprint' e1 ++ "/" ++ pprint' e2
>
>
> --helper fromJusts to raise errors
> fromJustErrA :: Maybe Arith -> Arith
> fromJustErrA (Just n) = n
> fromJustErrA Nothing = error "The expression you typed in is invalid" :: Arith
>
> fromJustErrE :: Maybe Double ->  Double
> fromJustErrE (Just n) = n
> fromJustErrE Nothing = error "The expression you typed in is invalid" :: Double



This is the main function that is called by `CalcREPL` to evaluate
user input.

> calc :: String -> String
> calc input = pprint' ( fromJustErrA $ pprintHelper input) ++ "\n\t= " ++ show  (fromJustErrE $ eval input)
