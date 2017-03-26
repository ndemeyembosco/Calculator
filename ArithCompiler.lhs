Project 1: Arith compiler
=========================

We won't spend much time in this course talking about compilers.  But
for this first project you will explore a very simple compiler for the
Arith language.

**Remember that you must complete this project independently.  The
project is due Thursday, September 15, at 1:15pm.**

First, some extensions and imports we will need for the parser.  If
you get errors about modules not being found, run `cabal install
parsec` from the command line.

> {-# LANGUAGE GADTs #-}
>
> import           Text.Parsec          hiding (Error, many, (<|>))
> import           Text.Parsec.Expr
> import           Text.Parsec.Language (emptyDef)
> import           Text.Parsec.String   (Parser)
> import qualified Text.Parsec.Token    as P
> import           Control.Applicative

Here are the data types we used to represent Arith abstract syntax in
class, along with a simple interpreter.

> data Op where
>   Plus  :: Op
>   Minus :: Op
>   Times :: Op
>   deriving (Show, Eq)
>
> data Arith where
>   Lit :: Integer -> Arith
>   Bin :: Op -> Arith -> Arith -> Arith
>   deriving (Show)
>
> interp :: Arith -> Integer
> interp (Lit n)        = n
> interp (Bin op a1 a2) = interpOp op (interp a1) (interp a2)
>
> interpOp :: Op -> Integer -> Integer -> Integer
> interpOp Plus  = (+)
> interpOp Minus = (-)
> interpOp Times = (*)

The abstract stack machine
--------------------------

Instead of compiling Arith programs to machine code, you will compile
them to an *abstract machine*.  An abstract machine is just like a
real machine except for the fact that it is imaginary.

Our imaginary machine is quite simple.  It keeps track of a list of
instructions to execute, and a stack of integers.  There are four
instructions it knows how to execute:

+ `PUSH n`: given an integer `n`, push it on top of the stack.
+ `ADD`: pop the top two integers off the stack, add them, and push
  the result back on top of the stack.  The machine halts with an
  error if there are fewer than two integers on the stack.
+ `SUB`: pop the top two integers, subtract the topmost from the
  other, and push the result.
+ `MUL`: pop the top two integers, multiply them, and push the result.


1. **Make a data type called `Instruction` to represent the four stack
    machine instructions described above.**

> type Stack = [Integer]


> data Instruction where
>   PUSH :: Integer -> Instruction
>   ADD :: Instruction
>   SUB :: Instruction
>   MUL :: Instruction
>   deriving Show

Our machine can also be in one of three states:

+ `WORKING`: the machine has some next instructions to execute and a stack.
+ `DONE`: in this case there are no more instructions to execute.  The
  machine remembers only the final stack.
+ `ERROR`: something has gone terribly, horribly wrong. In this state,
    the machine does not need to remember any instructions or stack.

2. **Make a data type called `MachineState` to represent the possible
   states of the machine, as described above.  Each different state
   should contain whatever information the machine needs to remember
   in that state.**

> data MachineState where
>    WORKING :: Instruction -> Stack -> MachineState
>    DONE :: Stack -> MachineState
>    ERROR :: MachineState
>    deriving Show

3. **Write a function `step :: MachineState -> MachineState` which
   executes a single step of the machine.  For example, in the
   `WORKING` state it should execute the next instruction and return
   an appropriate next state for the machine.**

> step :: MachineState -> MachineState
> step (WORKING inst []) = case inst of
>    PUSH n -> DONE [n]
>    _      -> ERROR
> step (WORKING inst (x:xs)) = case inst of
>    PUSH n -> DONE (n:x:xs)
>    ADD    -> case xs of
>                [] -> ERROR
>                (t:ts) -> DONE ((x + t) : ts)
>    SUB    -> case xs of
>                [] -> ERROR
>                (t:ts) -> DONE ((x - t) : ts)
>    MUL    -> case xs of
>                [] -> ERROR
>                (t:ts) -> DONE ((x * t) : ts)
> step mstate = mstate





4. **Write `execute :: [Instruction] -> MachineState`, which takes a
   program and runs the machine (starting with an empty stack) until
   the machine won't run anymore (that is, it has reached a `DONE` or
   `ERROR` state).**

> executeHelper :: [Instruction] -> Stack -> MachineState
> executeHelper [] m = DONE m
> executeHelper (inst: rest) stack = case (step (WORKING inst stack)) of
>     ERROR -> ERROR
>     DONE stack' -> executeHelper rest stack'
>
> execute :: [Instruction] -> MachineState
> execute m = executeHelper m []






5. **Finally, write `run :: [Instruction] -> Maybe Integer`, which
   executes the program and then returns `Nothing` if the machine
   halted with an `ERROR` or an empty stack, or `Just` the top integer
   on the stack if the machine successfully finished and left at least
   one integer on the stack.**



> run :: [Instruction] -> Maybe Integer
> run insts = case execute insts of
>      ERROR -> Nothing
>      DONE [] -> Nothing
>      DONE (x:_) -> Just x


The compiler
------------

Now that you have a working abstract machine, you can compile Arith
expressions into equivalent programs that run on the abstract machine.

**Write a function `compile` which takes an `Arith` and yields a
list of `Instruction`s.**

Of course, your compiler should output not just *any* list of
instructions!  It should output a program which, when run on the
abstract machine, successfully produces the same integer result as the
Arith interpreter would.  That is, for any `a :: Arith`,

```
run (compile a) == Just (interp a)

```

> opInstEquiv :: Op -> Instruction
> opInstEquiv Plus = ADD
> opInstEquiv Times = MUL
> opInstEquiv Minus = SUB

> compile :: Arith -> [Instruction]
> compile (Lit n) = [PUSH n]
> compile (Bin op l r) = (compile l) ++ (compile r)++ [opInstEquiv op]

> compTest :: Bool
> compTest = run (compile a) == Just (interp a)
>                where
>                   a = Bin Times (Bin Plus (Lit 3) (Lit 4)) (Lit 5)

Optional extensions
-------------------

1. Try extending Arith along with the parser, interpreter, and
   compiler with more operations (for example, modulus or
   exponentiation).  To extend the parser you should only have to edit
   the definition of `table`, which has an explanatory comment.

Parser
------

This parser is provided for your convenience, to help you test your functions.
Use the `readArith` function to parse concrete Arith syntax into an AST.

> readArith :: String -> Arith
> readArith s = case parse parseArith "" s of
>   Left  err -> error (show err)
>   Right a   -> a

[Pay no attention to the man behind the curtain](https://www.youtube.com/watch?v=YWyCCJ6B2WE)...

> lexer :: P.TokenParser u
> lexer = P.makeTokenParser $
>   emptyDef
>   { P.opStart         = oneOf "+-*"
>   , P.opLetter        = oneOf "+-*"
>   , P.reservedOpNames = ["+", "-", "*"]
>   }
>
> parens :: Parser a -> Parser a
> parens     = P.parens lexer
>
> reservedOp :: String -> Parser ()
> reservedOp = P.reservedOp lexer
>
> integer :: Parser Integer
> integer = P.integer lexer
>
> whiteSpace :: Parser ()
> whiteSpace = P.whiteSpace lexer
>
> parseAtom :: Parser Arith
> parseAtom = Lit <$> integer <|> parens parseExpr
>
> parseExpr :: Parser Arith
> parseExpr = buildExpressionParser table parseAtom <?> "expression"
>   where
>     -- Each list of operators in the table has the same precedence, and
>     -- the lists are ordered from highest precedence to lowest.  So
>     -- in this case '*' has the highest precedence, and then "+" and
>     -- "-" have lower (but equal) precedence.
>     table = [ [ binary "*" (Bin Times) AssocLeft ]
>             , [ binary "+" (Bin Plus)  AssocLeft
>               , binary "-" (Bin Minus) AssocLeft
>               ]
>             ]
>
>     binary name fun assoc = Infix (reservedOp name >> return fun) assoc
>
> parseArith :: Parser Arith
> parseArith = whiteSpace *> parseExpr <* eof
