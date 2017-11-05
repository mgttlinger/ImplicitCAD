-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2017, Merlin Göttlinger (megoettlinger@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE

{-# LANGUAGE Rank2Types #-}

module Graphics.Implicit.ExtOpenScad.Parser.Statement where

import Prelude(Bool(False, True), Either, String, Maybe(Just, Nothing), pure, ($), (<*), (*>), (<*>), (<$>))

import Text.ParserCombinators.Parsec (Column, Line, SourceName, ParseError, eof, getPosition, many, many1, noneOf, optional, oneOf, parse, sepBy, sourceColumn, sourceLine, space, string, try, (<|>))

import Graphics.Implicit.ExtOpenScad.Definitions (Pattern(Name), Statement(DoNothing, NewModule, Include, Echo, If, For, ModuleCall,(:=)),Expr(LamE), StatementI(StatementI))
import Graphics.Implicit.ExtOpenScad.Parser.Util (Parser, angles, braces, comma, commaSep, curly, equals, genSpace, tryMany, stringGS, pad, (*<|>), (?:), patternMatcher, variableSymb)
import Graphics.Implicit.ExtOpenScad.Parser.Expr (expr0)



parseProgram :: SourceName -> String -> Either ParseError [StatementI]
parseProgram = parse program
  where
    program :: Parser [StatementI]
    program = many1 computation <* eof

-- | A in our programming openscad-like programming language.
computation :: Parser StatementI
computation =
  pad $ tryMany [ -- suite statements: no semicolon...
    ifStatementI,
    forStatementI,
    throwAway,
    userModuleDeclaration{-,
    unimplemented "mirror",
    unimplemented "multmatrix",
    unimplemented "color",
    unimplemented "render",
    unimplemented "surface",
    unimplemented "projection",
    unimplemented "import_stl"-}
    -- rotateExtrude
    ]
  *<|> genSpace *> tryMany [ -- Non suite s. Semicolon needed...
    echo,
    include,
    function,
    assignment--,
    --use
    ] <* stringGS " ; "
  *<|> pad userModule -- Modules

{-
-- | A suite of s!
--   What's a suite? Consider:
--
--      union() {
--         sphere(3);
--      }
--
--  The suite was in the braces ({}). Similarily, the
--  following has the same suite:
--
--      union() sphere(3);
--
--  We consider it to be a list of s which
--  are in turn StatementI s.
--  So this parses them.
-}
suite :: Parser [StatementI]
suite = "suite" ?: (
  (pure <$> computation)
  <|> curly (pad $ many (try computation))
  )



columnNumber :: Parser Column
columnNumber = sourceColumn <$> getPosition

withLineNumber :: Parser (Statement StatementI) -> Parser StatementI
withLineNumber e = StatementI <$> lineNumber <*> e
  where
    lineNumber :: Parser Line
    lineNumber = sourceLine <$> getPosition

namedWithLN :: String -> Parser (Statement StatementI) -> Parser StatementI
namedWithLN n e = n ?: withLineNumber e

throwAway :: Parser StatementI
throwAway = withLineNumber $ genSpace *> oneOf "%*" *> genSpace *> computation *> pure DoNothing

-- An included ! Basically, inject another openscad file here...
include :: Parser StatementI
include = namedWithLN "include " (do
  injectVals <- (string "include" *> pure True )
                 <|> (string "use" *> pure False)
  filename <- angles $ many (noneOf "<> ")
  pure $ Include filename injectVals)

-- | An assignment  (parser)
assignment :: Parser StatementI
assignment = namedWithLN "assignment " ((:=) <$> patternMatcher <*> (equals *> expr0))

-- | A function declaration (parser)
function :: Parser StatementI
function = namedWithLN "function " (do
  varSymb <- optional (string "function" *> space) *> genSpace *> variableSymb
  argVars <- braces $ commaSep patternMatcher
  valExpr <- equals *> expr0
  pure $ Name varSymb := LamE argVars valExpr)

-- | An echo  (parser)
echo :: Parser StatementI
echo = namedWithLN "echo " (Echo <$> (stringGS "echo" *> braces (commaSep expr0)))

ifStatementI :: Parser StatementI
ifStatementI = namedWithLN "if " (do
  bexpr <- stringGS "if" *> braces expr0
  sTrueCase <- suite
  _ <- genSpace
  sFalseCase <- (stringGS "else " *> suite ) *<|> pure []
  pure $ If bexpr sTrueCase sFalseCase)

assignmentH :: Parser n -> Parser v -> Parser (n, v)
assignmentH np vp = do
  n <- np
  _ <- equals
  v <- vp
  pure (n, v)

patternExpr :: Parser (Pattern, Expr)
patternExpr = assignmentH patternMatcher expr0

forStatementI :: Parser StatementI
forStatementI = namedWithLN "for " (do
  -- a for loop is of the form:
  --      for ( vsymb = vexpr   ) loops
  -- eg.  for ( a     = [1,2,3] ) {echo(a);   echo "lol";}
  -- eg.  for ( [a,b] = [[1,2]] ) {echo(a+b); echo "lol";}
  (pat, expr) <- stringGS "for" *> braces patternExpr
  loopContent <- suite
  pure $ For pat expr loopContent)

userModule :: Parser StatementI
userModule = namedWithLN "user module " (do
  name <- variableSymb
  args <- pad moduleArgsUnit
  s <- suite *<|> (stringGS " ; " *> pure [])
  pure $ ModuleCall name args s)

userModuleDeclaration :: Parser StatementI
userModuleDeclaration = namedWithLN "user module decl " (do
  newModuleName <- stringGS "module " *> variableSymb
  args <- pad moduleArgsUnitDecl
  s <- suite
  pure $ NewModule newModuleName args s)

----------------------

moduleArgsUnit :: Parser [(Maybe String, Expr)]
moduleArgsUnit = "module args " ?: braces (sepBy (
  do
    -- eg. a = 12
    (symb, expr) <- assignmentH variableSymb expr0
    pure (Just symb, expr)
  *<|> do
    -- eg. a(x,y) = 12
    symb <- variableSymb
    argVars <- braces $ sepBy variableSymb (try $ stringGS " , ")
    _ <- equals
    expr <- expr0
    pure (Just symb, LamE (Name <$> argVars) expr)
  *<|> do
    -- eg. 12
    expr <- expr0
    pure (Nothing, expr)
  ) (try comma))

moduleArgsUnitDecl :: Parser [(String, Maybe Expr)]
moduleArgsUnitDecl = braces $ sepBy (
  do
    (symb, expr) <- assignmentH variableSymb expr0
    pure (symb, Just expr)
  *<|> do
    symb <- variableSymb;
    -- FIXME: why match this content, then drop it?
    _ <- braces $ sepBy variableSymb (try comma)
    _ <- equals
    expr <- expr0
    -- FIXME: this line looks right, but.. what does this change?
    --            pure $ (Just symb, LamE (map Name argVars) expr)
    pure (symb, Just expr)
  *<|> do
    symb <- variableSymb
    pure (symb, Nothing)
  ) (try comma)

