module Set
where
import           Control.Monad          (void)
import           Data.List              (intercalate)
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer  as Lexer
import           Text.Megaparsec.String (Parser)

data Expr a
    = Set [a]
    | Nested (Expr a)
    | Union (Expr a) (Expr a)
    | Intersection (Expr a) (Expr a)
    | Difference (Expr a) (Expr a)
    | SymmetricDifference (Expr a) (Expr a)
    deriving Show

eval :: Ord a => Expr a -> [a]
eval (Set a) = a
eval (Nested x) = eval x
eval (Union x y) = eval x ++ eval y
eval (Intersection x y) = filter (`elem` eval y) $ eval x
eval (Difference x y) = filter (`notElem` eval y) $ eval x
eval (SymmetricDifference x y) = eval $ Difference u i
  where
    u = Union x y
    i = Intersection x y

pretty :: [Int] -> String
pretty xs = "{" ++ intercalate ", " (map show xs) ++ "}"

spaceConsumer :: Parser ()
spaceConsumer = Lexer.space (void spaceChar) line block
  where
    line = Lexer.skipLineComment "//"
    block = Lexer.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme p = Lexer.lexeme spaceConsumer p

closed :: Char -> Parser a -> Char -> Parser a
closed o p c = open *> p <* close
  where
    open = lexeme $ char o
    close = lexeme $ char c

chainl :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl p op = p >>= rest
  where
    rest x = (op <*> pure x <*> p >>= rest) <|> pure x

int :: Parser Int
int = lexeme $ (read <$> some digitChar)

comma :: Parser ()
comma = lexeme $ void $ char ','

set :: Parser a -> Parser (Expr a)
set p = Set <$> closed '{' list '}'
  where
    list = sepBy p comma

nested :: Parser a -> Parser (Expr a)
nested p = Nested <$> closed '(' (expr p) ')'

operand :: Parser a -> Parser (Expr a)
operand p = nested p <|> set p

unionOp :: Parser (Expr a -> Expr a -> Expr a)
unionOp = Union <$ lexeme (char '+')

intersectionOp :: Parser (Expr a -> Expr a -> Expr a)
intersectionOp = Intersection <$ lexeme (char '^')

differenceOp :: Parser (Expr a -> Expr a -> Expr a)
differenceOp = Difference <$ lexeme (char '-')

symmetricDifferenceOp :: Parser (Expr a -> Expr a -> Expr a)
symmetricDifferenceOp = SymmetricDifference <$ lexeme (string "<>")

operator :: Parser (Expr a -> Expr a -> Expr a)
operator =   try unionOp
         <|> try intersectionOp
         <|> try differenceOp
         <|> symmetricDifferenceOp

expr :: Parser a -> Parser (Expr a)
expr p = operand p `chainl` operator

intExpr :: Parser (Expr Int)
intExpr = spaceConsumer *> expr int <* eof

exec :: String -> String
exec xs = case parse intExpr "<stdin>" xs of
    (Left er) -> parseErrorPretty er
    (Right x) -> pretty . eval $ x
