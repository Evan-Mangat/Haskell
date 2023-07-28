import Prelude
import Data.Char
import System.IO
import System.Environment
import Control.Applicative


{- Formula ::= 'T' | 'F' | Ident
    | '(' Formula ')'
    | '!' Formula
    | Formula '/\' Formula
    | Formula '\/' Formula
    | Formula '->' Formula
    | Formula '<->' Formula -}

data Prop = Const Bool
    | Var String
    | Not Prop
    | And Prop Prop
    | Or Prop Prop
    | Imply Prop Prop
    | Iff Prop Prop
    deriving (Eq, Read, Show)  


newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) input = p input


instance Functor Parser where
 -- fmap :: (a -> b) -> Parser a -> Parser b
 fmap f p = P (\input -> case parse p input of
                            [] -> []
                            [(v,out)] -> [(f v, out)])


instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\input -> [(v, input)])
    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> px = P (\input -> case parse pf input of
                                [] -> []
                                [(f,out)] -> parse (fmap f px) out)


instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\input -> case parse p input of
                              [] -> []
                              [(v,out)] -> parse (f v) out)


instance Alternative Parser where
 -- empty :: Parser a
 empty = P (\input -> [])
 -- (<|>) :: Parser a -> Parser a -> Parser a
 p <|> q = P (\input -> case parse p input of
                            [] -> parse q input
                            [(v,out)] -> [(v,out)])



item :: Parser Char
item = P (\input -> case input of
                      [] -> []
                      (x:xs) -> [(x, xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

char :: Char -> Parser Char
char c = P (\input -> case input of 
                        x : xs | x == c -> [(c, xs)]
                        _ -> [])

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x <- (sat isLower)
           xs <- many (sat isAlphaNum)
           return (x:xs)

nat :: Parser Int
nat = do xs <- some (sat isDigit)
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

-- Step 4
constant :: Parser Prop 
constant = (char 'T' >> return (Main.Const True))
        <|> (char 'F' >> return(Main.Const False))
-- Step 5
var :: Parser Prop
var = do c <- identifier
         return $ Var (c)
        <|> constant
         
-- Step 6
formula :: Parser Prop
formula = do i <- impterm
             symbol "<->"
             f <- formula
             return (Iff i f)
           <|> impterm

impterm :: Parser Prop
impterm = do o <- orthis 
             symbol "->"
             i <- impterm
             return (Imply o i)
           <|> orthis 

orthis :: Parser Prop
orthis = do a <- andthis
            symbol "\\/"
            o <- orthis 
            return (Or a o) 
          <|> andthis

andthis :: Parser Prop
andthis = do n <- notthis
             symbol "/\\"
             a <- andthis
             return (And n a)
           <|> notthis

notthis :: Parser Prop
notthis = do symbol "!"
             n <- notthis 
             return (Not n)
           <|> factor 

factor :: Parser Prop
factor = do symbol "("
            f <- formula
            symbol ")"
            return f
          <|> var
--
-- Step 7
parseFormula :: String -> String
parseFormula str =
  case parse formula str of
    [(prop, "")] -> show prop
    _ -> "Parse Error"


--
main :: IO ()
main = do
    arg <- getArgs
    input <- readFile (head arg)
    let formulas = lines input
        results = map parseFormula formulas
    mapM_ putStrLn results    
