module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    parseTest,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)
import Data.Bool (Bool)


-- Do not change this definition.
type Parser = Parsec Void String

keywords :: [String]
keywords = 
  [
    "if",
    "then",
    "else",
    "true",
    "false",
    "let",
    "in",
    "try",
    "catch",
    "print",
    "put",
    "get"
  ]

pExp :: Parser Exp
pExp = 
  choice
  [CstInt <$> lInteger,
  CstBool <$> lBool,
  Var <$> lVName
  ]

-- Do not change this definition.
parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x



lInteger :: Parser Integer
lInteger = lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlpha)

lexeme :: Parser a -> Parser a
lexeme p = p <* space

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let res = c : cs
  if res `elem` keywords
    then fail "Unexpected keyword"
    else pure res


lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

lBool :: Parser Bool
lBool =
  try $ lexeme $ 
    choice
      [
        const True <$> lKeyword "true",
        const False <$> lKeyword "false"
      ]

