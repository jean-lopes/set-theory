module Set2
where
import           Control.Monad          (void)
import           Data.List              (intercalate)
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer  as Lexer
import           Text.Megaparsec.String (Parser)

cartesianProduct :: [a] -> [a] -> [(a, a)]
cartesianProduct xs ys = [ (x, y) | x <- xs, y <- ys ]

data Expr a
    = Set [a]
    deriving Show

eval :: Ord a => Expr a -> [a]
eval (Set a) = a

pretty :: [Int] -> String
pretty xs = "{" ++ intercalate ", " (map show xs) ++ "}"

spaceConsumer :: Parser ()
spaceConsumer = Lexer.space (void spaceChar) line block
  where
    line = Lexer.skipLineComment "//"
    block = Lexer.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme p = Lexer.lexeme spaceConsumer p

chainl :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl p op = p >>= rest
    where
    rest x = (op <*> pure x <*> p >>= rest) <|> pure x

expr :: Parser a -> Parser (Expr a)
expr p = operand p `chainl` operator

intExpr :: Parser (Expr Int)
intExpr = spaceConsumer *> expr int <* eof

exec :: String -> String
exec xs = case parse intExpr "<stdin>" xs of
    (Left er) -> parseErrorPretty er
    (Right x) -> pretty . eval $ x

int :: Parser Int
int = undefined

comma :: Parser Char
comma = undefined

closed :: Char -> Parser a -> Char -> Parser a
closed _ _ = undefined

set :: Parser a -> Parser (Expr a)
set _ = undefined

nested :: Parser a -> Parser (Expr a)
nested _ = undefined

operand :: Parser a -> Parser (Expr a)
operand _ = undefined

unionOp :: Parser (Expr a -> Expr a -> Expr a)
unionOp = undefined

intersectionOp :: Parser (Expr a -> Expr a -> Expr a)
intersectionOp = undefined

differenceOp :: Parser (Expr a -> Expr a -> Expr a)
differenceOp = undefined

symmetricDifferenceOp :: Parser (Expr a -> Expr a -> Expr a)
symmetricDifferenceOp = undefined

operator :: Parser (Expr a -> Expr a -> Expr a)
operator = undefined
