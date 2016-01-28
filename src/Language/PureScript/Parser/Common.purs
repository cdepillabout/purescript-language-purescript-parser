
module Language.PureScript.Parser.Common where

import Prelude

import Control.Alt
import Control.MonadPlus
import Data.Array as Array
import Data.Either
import Data.Foldable
import Data.List (List)
import Data.Maybe
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.Pos as P
import Text.Parsing.Parser.String as P

import Language.PureScript.Comments
import Language.PureScript.Parser.Lexer
import Language.PureScript.Parser.State
import Language.PureScript.Names

properName :: TokenParser ProperName
properName = ProperName <$> uname

-- | Parse a module name
moduleName :: TokenParser ModuleName
moduleName = part []
  where
    part :: Array ProperName -> TokenParser ModuleName
    part path = (do name <- ProperName <$> P.try qualifier
                    part (path `snoc` name))
            <|> (ModuleName <<< snoc path <<< ProperName <$> mname)

    snoc :: Array ProperName -> ProperName -> Array ProperName
    snoc path name = path <> [name]

-- | -- Parse a qualified name, i.e. M.name or just name
parseQualified :: forall a . TokenParser a -> TokenParser (Qualified a)
parseQualified parser = part []
  where
    part :: Array ProperName -> TokenParser (Qualified a)
    part path = (do name <- ProperName <$> P.try qualifier
                    part (updatePath path name))
            <|> (Qualified (qual path) <$> P.try parser)

    updatePath :: Array ProperName -> ProperName -> Array ProperName
    updatePath path name = path <> [name]

    qual :: Array ProperName -> Maybe ModuleName
    qual path = if Array.null path then Nothing else Just $ ModuleName path

-- | Parse an identifier or parenthesized operator
parseIdent :: TokenParser Ident
parseIdent = (Ident <$> identifier) <|> (Op <$> parens symbol)

-- | Run the first parser, then match the second if possible, applying the specified function on a successful match
augment :: forall s m a b . (Monad m) => P.ParserT s m a -> P.ParserT s m b -> (a -> b -> a) -> P.ParserT s m a
augment p q f = flip (maybe id $ flip f) <$> p <*> P.optionMaybe q

-- | Run the first parser, then match the second zero or more times, applying the specified function for each match
fold :: forall s m a b . (Monad m) => P.ParserT s m a -> P.ParserT s m b -> (a -> b -> a) -> P.ParserT s m a
fold first more combine = do
  a <- first
  bs <- Array.many more
  return $ foldl combine a bs

-- | Build a parser from a smaller parser and a list of parsers for postfix operators
buildPostfixParser :: forall s m a . (Monad m) => Array (a -> P.ParserT s m a) -> P.ParserT s m a -> P.ParserT s m a
buildPostfixParser fs first = do
    a <- first
    go a
  where
    go a = do
        maybeA <- P.optionMaybe $ P.choice (map ($ a) fs)
        case maybeA of
            Nothing -> pure a
            Just a' -> go a'

-- | Mark the current indentation level
mark :: forall a . TokenParser a -> TokenParser a
mark p = do
  current <- _.indentationLevel <<< unParseState <$> getState
  pos <- (\(P.Position p) -> p.column) <$> getPosition
  updateIndentationLevel pos
  a <- p
  updateIndentationLevel current
  pure a

-- | Check that the current identation level matches a predicate
checkIndentation :: (Column -> Column -> Boolean) -> TokenParser Unit
checkIndentation rel = do
  col <- (\(P.Position p) -> p.column) <$> getPosition
  current <- _.indentationLevel <<< unParseState <$> getState
  guard (col `rel` current)

-- | Check that the current indentation level is past the current mark
indented :: TokenParser Unit
indented = checkIndentation (>) P.<?> "indentation"

-- | Check that the current indentation level is at the same indentation as the current mark
same :: TokenParser Unit
same = checkIndentation (==) P.<?> "no indentation"

-- | Read the comments from the the next token, without consuming it
-- readComments :: P.Parser (Array PositionedToken) (Array Comment)
readComments :: TokenParser (Array Comment)
readComments = P.lookAhead $ _.ptComments <<< unPositionedToken <$> anyToken

-- | Run a parser
runTokenParser :: forall a . TokenParser a -> List PositionedToken -> Either P.ParseError a
runTokenParser p posTokens = P.runParser parseStateWrapper p
  where
    parseStateWrapper :: ParseStateWrapper
    parseStateWrapper = ParseStateWrapper { parseState: initialParseState
                                          , tokens:     posTokens
                                          }
