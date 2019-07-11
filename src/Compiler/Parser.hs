-- parsing with megaparsec
-- https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
-- https://github.com/ucsd-cse131-fa18/06-fox/blob/master/lib/Language/Fox/Parser.hs

module Compiler.Parser 
  (SourceSpan(..)
  , parse
  ) where

import Control.Monad (void)
import Control.Exception hiding (try)

import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec.Char.Lexer as Lexer 
import qualified Text.Megaparsec.Char as Char
import Control.Monad.Combinators.Expr -- parser-combinators

import Data.List.NonEmpty as NonEmpty
import Data.Void

import Compiler.AST

type Parser = Parsec Void Text 

parse :: FilePath -> Text -> Program SourceSpan
parse f src = parseWith prog f src

parseWith :: Parser a -> FilePath -> Text -> a
parseWith p f s = case runParser (whole p) f s of 
                    Left err -> throw err
                    Right e -> e 


--------------------------------------------------------------------------------
-- | Source Span
--------------------------------------------------------------------------------
data SourceSpan = SourceSpan { spanStart :: !SourcePos, spanEnd :: !SourcePos }
  deriving (Eq, Show)

dummySpan :: SourceSpan
dummySpan = posSpan (initialPos "undefined")

posSpan :: SourcePos -> SourceSpan
posSpan p = SourceSpan p p 

instance Semigroup SourceSpan where
  x <> y = mappendSpan x y

instance Monoid SourceSpan where 
  mempty  = dummySpan


mappendSpan :: SourceSpan -> SourceSpan -> SourceSpan
mappendSpan s1 s2
  | s1 == dummySpan = s2
  | s2 == dummySpan = s1
  | otherwise       = SourceSpan (spanStart s1) (spanEnd s2)


--------------------------------------------------------------------------------
-- | Top-Level Expression Parser
--------------------------------------------------------------------------------
prog :: Parser (Program SourceSpan)
prog = Prog <$> many decl


decl :: Parser (Decl SourceSpan)
decl = withSpan' $ do 
  keyword "def"
  f  <- binder
  xs <- parens (sepBy binder comma) <* colon 
  e  <- expr
  return (Decl f xs e)

exprs :: Parser [Expr SourceSpan]
exprs = parens (sepBy1 expr comma)

expr :: Parser (Expr SourceSpan)
expr = makeExprParser term binops

term :: Parser (Expr SourceSpan)
term =  try primExpr
    <|> try letExpr
    <|> try ifExpr
    <|> try appExpr
    <|> try constExpr
    <|> idExpr


--------------------------------------------------------------------------------
-- | Individual Sub-Expression Parsers
--------------------------------------------------------------------------------
primExpr :: Parser (Expr SourceSpan)
primExpr = withSpan' (Prim1 <$> primOp <*> parens expr)

primOp :: Parser Prim1
primOp =  try (keyword "add1" *> pure Add1)
      <|> try (keyword "sub1" *> pure Sub1)
      <|> try (keyword "isZero"  *> pure IsZero)
      <|>     (keyword "not"  *> pure Not)


letExpr :: Parser (Expr SourceSpan)
letExpr = withSpan' $ do 
  keyword "let"
  bs <- sepBy1 bind comma
  keyword "in"
  e <- expr
  return (bindsExpr bs e)

bind :: Parser (Bind SourceSpan, Expr SourceSpan)
bind = (,) <$> binder <* symbol "=" <*> expr

ifExpr :: Parser (Expr SourceSpan)
ifExpr = withSpan' $ do
  keyword "if"
  b <- expr
  e1 <- between colon elsecolon expr
  e2 <- expr
  return (If b e1 e2)
  where 
    elsecolon = keyword "else" *> colon

funExpr :: Parser (Expr SourceSpan)
funExpr = idExpr

appExpr :: Parser (Expr SourceSpan)
appExpr = withSpan' (App <$> (fst <$> identifier) <*> exprs)


binops :: [[Operator Parser (Expr SourceSpan)]]
binops = [ [ InfixL (symbol "*" *> pure (op Times))
           ]
         , [ InfixL (symbol "+" *> pure (op Plus))
           , InfixL (symbol "-" *> pure (op Minus))
           ]
         , [ InfixL (symbol "==" *> pure (op Equal))
           , InfixL (symbol ">"  *> pure (op Greater))
           , InfixL (symbol "<"  *> pure (op Less))
           ]
         ]
  where
    op o e1 e2 = Prim2 o e1 e2 (stretch [e1, e2])

idExpr :: Parser (Expr SourceSpan)
idExpr = uncurry Id <$> identifier

constExpr :: Parser (Expr SourceSpan)
constExpr
   =  (uncurry Number <$> integer)
  <|> (Boolean True   <$> keyword "true")
  <|> (Boolean False  <$> keyword "false")

binder :: Parser (Bind SourceSpan)
binder =  try (uncurry Bind <$> identifier)
      <|> try (uncurry (Parameter True)  <$> (keyword "public"  *> identifier))
      <|>     (uncurry (Parameter False) <$> (keyword "private" *> identifier))

--------------------------------------------------------------------------------
-- | Tokenisers and Whitespace
--------------------------------------------------------------------------------

-- | Top-level parsers (should consume all input)
whole :: Parser a -> Parser a
whole p = space *> p <* eof

space :: Parser ()
space = Lexer.space (void Char.spaceChar) lineCmnt blockCmnt
  where
    lineCmnt  = Lexer.skipLineComment "//"
    blockCmnt = Lexer.skipBlockComment "/*" "*/" 

-- | 'symbol s' parses the string s and trailing whitespace
symbol :: String -> Parser String
symbol = Lexer.symbol space

comma :: Parser String
comma = symbol ","

colon :: Parser String
colon = symbol ":"

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | 'parens' parses something between parenthesis.
brackets :: Parser a -> Parser a
brackets  = between (symbol "[") (symbol "]")


withSpan' :: Parser (SourceSpan -> a) -> Parser a
withSpan' p = do 
  p1 <- getSourcePos
  f  <- p 
  p2 <- getSourcePos
  return (f (SourceSpan p1 p2))


withSpan :: Parser a -> Parser (a, SourceSpan)
withSpan p = do 
  pos1 <- getSourcePos
  x    <- p
  pos2 <- getSourcePos
  return (x, (SourceSpan pos1 pos2))


-- | 'lexeme p' consumes whitespace after running p
lexeme :: Parser a -> Parser (a, SourceSpan)
lexeme p = Lexer.lexeme space (withSpan p)


-- | 'integer' parses an integer
integer :: Parser (Integer, SourceSpan)
integer = lexeme Lexer.decimal

keyword :: String -> Parser SourceSpan
keyword w = snd <$> (withSpan (Char.string w) <* notFollowedBy Char.alphaNumChar <* space)

-- | list of reserved words
keywords :: [Text]
keywords =
  [ "if"     , "else"
  , "true"   , "false"
  , "let"    , "in"
  , "add1"   , "sub1"   , "isNum", "isBool", "isTuple", "print", "isZero", "not"
  , "def"    , "lambda"
  , "public" , "private"
  ]

-- | 'identifier' parses identifiers: lower-case alphabets followed by alphas or digits
identifier :: Parser (String, SourceSpan)
identifier = lexeme (p >>= check)
  where 
    p = (:) <$> Char.letterChar <*> many Char.alphaNumChar
    check x = if x `elem` keywords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x


stretch :: (Monoid a) => [Expr a] -> a
stretch = mconcat . fmap getLabel





