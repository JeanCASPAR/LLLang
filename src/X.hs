{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module X where

import Control.Applicative.Combinators hiding (many, some)
import Control.Monad.Combinators.Expr
import Data.Char (chr, ord)
import Data.Functor (($>))
import qualified Data.HashMap.Strict as H
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, singleton, unpack)
import Text.Megaparsec hiding (sepBy)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type TIdent = String

type TVar = Int

type Parser = Parsec String Text

data Type
  = TAtom TIdent
  | TNegAtom TIdent
  | TTypeVar TVar
  | TNegTypeVar TVar
  | TZero -- Never, !
  | TOne -- Unit, ()
  | TTop
  | TBottom -- Wait
  | TWith (H.HashMap TIdent Type)
  | TPlus (H.HashMap TIdent Type)
  | TTensor [Type]
  | TPar [Type]
  | TWhyNot Type
  | TOfCourse Type
  | TForall TVar Type -- forall a. A
  | TExists TVar Type -- exists a. A
  | TRecursive TVar Type -- mu a. A
  | TInt
  | TNegInt
  deriving (Show)

fun :: Type -> Type -> Type
fun a b = TPar [neg a, b]

neg :: Type -> Type
neg (TAtom ident) = TNegAtom ident
neg (TNegAtom ident) = TAtom ident
neg (TTypeVar var) = TNegTypeVar var
neg (TNegTypeVar var) = TTypeVar var
neg TZero = TTop
neg TOne = TBottom
neg TBottom = TOne
neg TTop = TZero
neg (TWith fields) = TPlus (H.map neg fields)
neg (TPlus fields) = TWith (H.map neg fields)
neg (TTensor ty) = TPar (fmap neg ty)
neg (TPar ty) = TTensor (fmap neg ty)
neg (TWhyNot ty) = TOfCourse (neg ty)
neg (TOfCourse ty) = TWhyNot (neg ty)
neg (TForall var ty) = TExists var (neg ty)
neg (TExists var ty) = TForall var (neg ty)
neg (TRecursive var ty) = TRecursive var (neg ty)
neg TInt = TNegInt
neg TNegInt = TInt

data Context = Context
  { nextFreeVar :: Int,
    types :: H.HashMap TIdent Type,
    varNames :: [String]
  }

empty :: Context
empty =
  Context
    { nextFreeVar = 0,
      types = H.empty,
      varNames = []
    }

endIdentParser :: Parser TIdent
endIdentParser = many alphaNumChar

totalIdentParser :: Parser Char -> Parser TIdent
totalIdentParser p = do
  firstLetter <- p
  rest <- endIdentParser
  return (firstLetter : rest)

identParser :: Parser TIdent
identParser = totalIdentParser letterChar

typeAtomParser :: Context -> Parser (Type, Context)
typeAtomParser ctx = (,ctx) . TAtom <$> totalIdentParser upperChar

typeVarParser :: Context -> Parser (Type, Context)
typeVarParser ctx = do
  ident <- totalIdentParser lowerChar
  let (var, newCtx) =
        case elemIndex ident (varNames ctx) of
          Just idx -> (idx, ctx)
          Nothing ->
            let name = nextTypeVar (varNames ctx)
                newCtx' = ctx {nextFreeVar = nextFreeVar ctx + 1, varNames = name : varNames ctx}
             in (nextFreeVar newCtx', newCtx)
   in return (TTypeVar var, newCtx)
  where
    nextTypeVar :: [String] -> String
    nextTypeVar [] = "a"
    nextTypeVar l = addOne (last l)

    addOne :: String -> String
    addOne (h : t) =
      if h `elem` ['a' .. 'y']
        then chr (ord h + 1) : t
        else 'a' : addOne t
    addOne [] = error "Should be non-empty"

prefixParser :: (Context -> Parser (Char, Context)) -> Context -> Parser (Type, Context)
prefixParser p ctx = do
  (_, newCtx) <- p ctx
  typeParser newCtx

negParser :: Context -> Parser (Type, Context)
negParser ctx = do
  (ty, newCtx) <- prefixParser (\context -> (,context) <$> single '~') ctx
  return (neg ty, newCtx)

ofCourseParser :: Context -> Parser (Type, Context)
ofCourseParser ctx = do
  (ty, newCtx) <- prefixParser (\context -> (,context) <$> single '!') ctx
  return (TOfCourse ty, newCtx)

whyNotParser :: Context -> Parser (Type, Context)
whyNotParser ctx = do
  (ty, newCtx) <- prefixParser (\context -> (,context) <$> single '!') ctx
  return (TWhyNot ty, newCtx)

forallParser :: Context -> Parser (Type, Context)
forallParser ctx = do
  _ <- string "forall" <|> (singleton <$> single '∀')
  (TTypeVar var, fstCtx) <- typeVarParser ctx
  _ <- single '.'
  (ty, sndCtx) <- typeParser fstCtx
  return (TForall var ty, sndCtx)

existsParser :: Context -> Parser (Type, Context)
existsParser ctx = do
  _ <- string "exists" <|> (singleton <$> single '∃')
  (TTypeVar var, fstCtx) <- typeVarParser ctx
  _ <- single '.'
  (ty, sndCtx) <- typeParser fstCtx
  return (TExists var ty, sndCtx)

recursiveTypeParser :: Context -> Parser (Type, Context)
recursiveTypeParser ctx = do
  _ <- string "rec"
  (TTypeVar var, fstCtx) <- typeVarParser ctx
  _ <- single '.'
  (ty, sndCtx) <- typeParser fstCtx
  return (TRecursive var ty, sndCtx)

recordParser :: Context -> Parser (H.HashMap TIdent Type, Context)
recordParser ctx = do
  fields <-
    braces
      ( commaSep
          ( do
              ident <- totalIdentParser upperChar
              _ <- symbol ":"
              -- We can drop the context because we close a scope
              (ty, _) <- typeParser ctx
              return (ident, ty)
          )
      )
  return (H.fromList fields, ctx)

withParser :: Context -> Parser (Type, Context)
withParser ctx = do
  _ <- single '&'
  (fields, newCtx) <- recordParser ctx
  return (TWith fields, newCtx)

plusParser :: Context -> Parser (Type, Context)
plusParser ctx = do
  _ <- single '+'
  (fields, newCtx) <- recordParser ctx
  return (TPlus fields, newCtx)

tensorParser :: Context -> Parser (Type, Context)
tensorParser ctx =
  parens
    ( do
        types <- commaSep (typeParser ctx)
        -- We can drop the context because we close a scope
        return (TTensor (fmap fst types), ctx)
    )

parParser :: Context -> Parser (Type, Context)
parParser ctx = do
  types <- typeParser ctx `sepBy` symbol "|"
  -- We can drop the context because we close a scope
  return (TTensor (fmap fst types), ctx)

intTypeParser :: Context -> Parser (Type, Context)
intTypeParser ctx = do
  _ <- string "Int"
  return (TInt, ctx)

typeParser :: Context -> Parser (Type, Context)
typeParser ctx =
  choice
    . fmap
      ($ ctx)
    $ [ negParser,
        ofCourseParser,
        whyNotParser,
        forallParser,
        existsParser,
        recursiveTypeParser,
        -- TODO: ajouter comme check "!= de forall, exists, rec, int"
        typeAtomParser,
        typeVarParser,
        withParser,
        plusParser,
        tensorParser,
        parParser,
        intTypeParser
      ]

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol s = do
  tok <- L.symbol sc (pack s)
  return . unpack $ tok

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

integer :: Parser Integer
integer = lexeme L.decimal

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` symbol ","

exampleTypeIntList :: Type
exampleTypeIntList = TRecursive 0 (TPlus (H.fromList [("Nil", TTypeVar 0), ("Cons", TPar [TNegInt, TNegTypeVar 0, TTypeVar 0])]))

exampleParse :: IO ()
exampleParse =
  let res = parse (typeParser X.empty) "example" "rec a . +{\"Nil\": a, \"Cons\": ~Int | ~a | a}"
   in case res of
        Left err -> print err
        Right (ty, _) -> print ty
