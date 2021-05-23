module Parser (Parser) where

import Data.Text (Text)
import Data.Map
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative
import Control.Applicative.Combinators

type Parser = Parsec String Text

type Ident = String

data Type =
    | Named Ident
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
    | Bottom.

data TypeDecl = Decl {
    name :: Ident,
    attrs :: Map Ident Type
}

data FunDecl = {
    name :: Ident,
    -- Idents are assumed to be unique
    params :: [(Ident, Type)]
    ret :: Type
    code :: AST
}

data AST = ()

data Toplevel =
    | Type TypeDecl
    | Function FunctionDecl
