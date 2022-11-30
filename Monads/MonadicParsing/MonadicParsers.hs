import Control.Applicative
import Data.Char

-- Basic definitions and a parser for strings.

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- Sequencing and Making Choices between Parsers.

instance Functor Parser where
  fmap g p = P (\inp -> case parse p inp of
                           []         -> []
                           [(v, out)] -> [(g v, out)])

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])

  pg <*> px = P (\inp -> case parse pg inp of
                  []         -> []
                  [(g, out)] -> parse (fmap g px) out)

-- Example of the applicative primitive in action.

three :: Parser (Char, Char)
three = pure g <*> item <*> item <*> item
        where g x y z = (x, z)

instance Monad Parser where
  -- That is, the parser p >>= f fails if the application of the parser p to the input
  -- string inp fails, and otherwise applies the function f to the result value v to give
  -- another parser f v, which is then applied to the output string out that was produced by
  -- the first parser to give the final result.
  p >>= f = P (\inp -> case parse p inp of
                          []         -> []
                          [(v, out)] -> parse (f v) out)

three' :: Parser (Char, Char)
three' = do 
            x <- item
            y <- item
            z <- item
            return (x, z)
