{-# LANGUAGE OverloadedStrings #-}

module Tansit.Parser (parseLongList) where

import Control.Applicative
import Control.Monad (void)
import Data.Attoparsec.Text
import Data.Char (ord)
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import qualified Data.Map as Map
import qualified Data.Text as Text

parseLongList :: Text -> Either String [Map Text Text]
parseLongList = parseOnly (p <* endOfInput)
  where p = do
          {- note only one \n here as one has already
           - been consumed by endOfLineOrInput below. -}
          pkgs <- sepBy (commentsBlock <|> pkg) "\n"
          return $ filter (not . Map.null) pkgs
        pkg = Map.fromList <$> many' propWithComments
        commentsBlock = many1Comments >> return Map.empty
        propWithComments = manyComments *> prop <* manyComments
        prop = (,) <$> propKey <*> propValue
        propKey = takeWhile1 charIsFieldName <* char ':' <* skipSpace
        propValue = do
          f <- restOfLine <|> (endOfLineOrInput >> return "")
          fs <- many' valOtherLine
          return $ Text.intercalate "\n" $ f : fs
        valOtherLine = (char ' ' <|> char '\t') >> (docBlankLine <|> restOfLine)
        docBlankLine = char '.' >> endOfLineOrInput >> return ""
        manyComments = void $ many commentLine
        many1Comments = void $ many1 commentLine
        commentLine = void $ char '#' >> (void restOfLine <|> endOfLineOrInput)
        restOfLine = takeWhile1 (not . isEndOfLine) <* endOfLineOrInput
        endOfLineOrInput = endOfLine <|> endOfInput
        -- This is from the Debian policy manual:
        -- https://www.debian.org/doc/debian-policy/ch-controlfields.html
        charIsFieldName c = let oc = ord c in
          (oc >= 33 && oc <= 57) || (oc >= 59 && oc <= 126)

