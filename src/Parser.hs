{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser (Parser) where

import Control.Applicative.Combinators hiding (many, some)
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Data.Map
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec String Text

type Ident = String

parseIdent :: Parser Ident
parseIdent =
  do
    firstLetter <- letterChar
    rest <- many alphaNumChar
    return (firstLetter : rest)

data Type
  = Named Ident
  | With Type Type
  | Plus Type Type
  | Tensor Type Type
  | Par Type Type
  | WhyNot Type
  | OfCourse Type
  | Fun Type Type
  | Zero
  | One
  | Top
  | Bottom
  | Forall Ident Type -- forall a. A
  | Exists Ident Type -- exists a. A

typeOperators :: [[Operator Parser Type]]
typeOperators =
  [ [prefix "!" OfCourse, prefix "?" WhyNot],
    [binary "&" With, binary "(+)" Plus, binary "(x)" Tensor, binary "|" Par],
    [binary "-°" Fun],
    [quantifier "forall" Forall, quantifier "exists" Exists]
  ]
  where
    singleUnaryOp :: Text -> (Type -> Type) -> Parser (Type -> Type)
    singleUnaryOp symbol = (<$ L.symbol space symbol)

    manyUnaryOp :: Text -> (Type -> Type) -> Parser (Type -> Type)
    manyUnaryOp symbol f = foldr1 (.) <$> some (singleUnaryOp symbol f)

    prefix :: Text -> (Type -> Type) -> Operator Parser Type
    prefix symbol f = Prefix (manyUnaryOp symbol f)

    binary :: Text -> (Type -> Type -> Type) -> Operator Parser Type
    binary symbol = InfixL . (<$ L.symbol space symbol)

    quantifier :: Text -> (Ident -> Type -> Type) -> Operator Parser Type
    quantifier symbol kind = Prefix $ do
      L.symbol space1 symbol
      name <- parseIdent
      L.symbol space "."
      pure . kind $ name

typeParser :: Parser Type
typeParser =
  makeExprParser term typeOperators
  where
    symbol = L.symbol space
    parens = between (symbol "(") (symbol ")")
    term =
      choice
        [ symbol "1" $> One,
          symbol "0" $> Zero,
          symbol "T" $> Top,
          symbol "B" $> Bottom,
          parens typeParser,
          Named <$> parseIdent
        ]

data TypeDecl = MkTypeDecl
  { typeName :: Ident,
    attrs :: Map Ident Type
  }

data FunDecl = MkFunDecl
  { funName :: Ident,
    -- Idents are assumed to be unique in params
    params :: [(Ident, Type)],
    ret :: Type,
    code :: AST
  }

data AST
  = Let Ident (Maybe Type) AST
  | Call AST AST
  | Parenthesized AST
  | IntValue Int
  | BoolValue Bool
  | NamedValue Ident

data Toplevel
  = Type TypeDecl
  | Function FunDecl
