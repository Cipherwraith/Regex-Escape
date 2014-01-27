{-# LANGUAGE OverloadedStrings #-}
module Escaper where
import qualified Data.Text.Lazy as T

-- Example:
-- *Escaper> escapeRegex $ T.pack "!@#$%^&*()_+{}|\\][';\":?></.,"
-- "!@#\\$%\\^&\\*\\(\\)_\\+\\{}\\|\\\\]\\[';\":\\?></\\.,"


escapeRegex :: T.Text -- Text to search within
            -> T.Text -- Returned text with characters escaped
escapeRegex = escapeChars metachars

escapeChars :: [T.Text] -- meta chars
            -> T.Text   -- Input text
            -> T.Text   -- Output text
escapeChars [] inputText = inputText
escapeChars (m:ms) inputText = escapeChars ms $ escapeMetaChar inputText m


escapeMetaChar :: T.Text -- input text
               -> T.Text -- Character to search for
               -> T.Text -- Returned text with characters escaped
escapeMetaChar inputText textToSearchFor = T.replace textToSearchFor replacementText inputText
  where
    replacementText = "\\" `T.append` textToSearchFor

metachars :: [T.Text]
metachars = [ "\\" , "|" , "(" , ")" , "[" , "{" , "^" , "$" , "*" , "+" , "?" , "." ]
