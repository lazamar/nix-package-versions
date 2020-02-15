{-| Parsers using in Nix.Versions
-}
module Nix.Versions.Parsers
    ( hash
    , version
    , filePath
    , inQuotes
    ) where

import Control.Applicative ((<|>))
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Text (pack)
import Nix.Versions.Types (Hash(..), Version(..))
import Text.Parsec (ParseError, many, many1, sepBy, parse, oneOf, between)
import Text.Parsec.Char (alphaNum, char, spaces, string, digit)
import Text.Parsec.String (Parser)

-- | Parse a SHA1 hash
hash :: Parser Hash
hash = Hash . pack <$> many alphaNum

-- | A package version number.
-- May look like one of these:
--      5
--      5.0.0.2
--      v13.5
--      8.2.1-rc3
version :: Parser Version
version = Version . pack <$> aName

inQuotes :: Parser a -> Parser a
inQuotes = between (char '"') (char '"')

filePath :: Parser FilePath
filePath = aName `separatedBy` '/'

separatedBy :: Parser String -> Char -> Parser String
separatedBy p separator =
    fold . intersperse [separator] <$> p `sepBy` char separator

-- | A valid directory/file name
aName :: Parser String
aName = many1 (alphaNum <|> char '-' <|> char '_' <|> char ' ' <|> char '.')

