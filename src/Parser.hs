{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser (Parser) where

import AST
import Control.Applicative.Combinators hiding (many, some)
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Data.Map
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type TIdent = String

data Type
  = TNamed TIdent
  | TWith Type Type
  | TPlus Type Type
  | TTensor Type Type
  | TPar Type Type
  | TWhyNot Type
  | TOfCourse Type
  | TFun Type Type
  | TZero
  | TOne
  | TTop
  | TBottom
  | TForall TIdent Type -- forall a. A
  | TExists TIdent Type -- exists a. A
  deriving (Eq)

type Parser = Parsec String Text

parseIdent :: Parser TIdent
parseIdent =
  do
    firstLetter <- letterChar
    rest <- many alphaNumChar
    return (firstLetter : rest)

typeOperators :: [[Operator Parser Type]]
typeOperators =
  [ [prefix "!" TOfCourse, prefix "?" TWhyNot],
    [binary "&" TWith, binary "(+)" TPlus, binary "(x)" TTensor, binary "|" TPar],
    [binary "-°" TFun],
    [quantifier "forall" TForall, quantifier "exists" TExists]
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

    quantifier :: Text -> (TIdent -> Type -> Type) -> Operator Parser Type
    quantifier symbol kind = Prefix $ do
      _ <- L.symbol space1 symbol
      name <- parseIdent
      _ <- L.symbol space "."
      pure . kind $ name

typeParser :: Parser Type
typeParser =
  makeExprParser term typeOperators
  where
    symbol = L.symbol space
    parens = between (symbol "(") (symbol ")")
    term =
      choice
        [ symbol "1" $> TOne,
          symbol "0" $> TZero,
          symbol "T" $> TTop,
          symbol "B" $> TBottom,
          parens typeParser,
          TNamed <$> parseIdent
        ]

data TypeDecl = MkTypeDecl
  { typeName :: TIdent,
    attrs :: Map TIdent Type
  }

data FunDecl = MkFunDecl
  { funName :: TIdent,
    -- Idents are assumed to be unique in params
    params :: [(TIdent, Type)],
    ret :: Type,
    code :: AST
  }

data AST
  = Let TIdent (Maybe Type) AST
  | Call AST AST
  | Parenthesized AST
  | IntValue Int
  | BoolValue Bool
  | NamedValue TIdent

data Toplevel
  = Type TypeDecl
  | Function FunDecl
