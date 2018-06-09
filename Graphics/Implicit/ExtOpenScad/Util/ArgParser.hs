-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Needed to have access to the parametric type of a function signature inside the body
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Implicit.ExtOpenScad.Util.ArgParser where

import Prelude(String, Maybe(Just, Nothing), Int, ($), (++), concat, show, error, return, map, snd, filter, (.), fst, foldl1, not, null, (&&))

import Graphics.Implicit.ExtOpenScad.Definitions (ArgParser(AP, APTest, APBranch, APTerminator, APFailIf, APExample), OVal (OError), TestInvariant(EulerCharacteristic))
import Graphics.Implicit.ExtOpenScad.Util.OVal (fromOObj, toOObj, OTypeMirror)

import qualified Data.Map as Map
import Data.Maybe (isNothing, fromJust, isJust)

-- * ArgParser building functions

-- ** argument and combinators

argument :: forall desiredType. (OTypeMirror desiredType) => String -> ArgParser desiredType
argument name =
    AP name Nothing "" $ \oObjVal -> do
        let
            val :: Maybe desiredType -- this requires ScopedTypeVariables
            val = fromOObj oObjVal
            errmsg = case oObjVal of
                OError errs -> "error in computing value for arugment " ++ name
                             ++ ": " ++ concat errs
                _   ->  "arg " ++ show oObjVal ++ " not compatible with " ++ name
        -- Using /= Nothing would require Eq desiredType
        APFailIf (isNothing val) errmsg $ APTerminator $ fromJust val

doc :: ArgParser a -> String -> ArgParser a
doc (AP name defMaybeVal _ next) newDoc = AP name defMaybeVal newDoc next
doc _ _ = error "Impossible!"

defaultTo :: (OTypeMirror a) => ArgParser a -> a -> ArgParser a
defaultTo (AP name _ doc' next) newDefVal =
    AP name (Just $ toOObj newDefVal) doc' next
defaultTo _ _ = error "Impossible!"

-- ** example

example :: String -> ArgParser ()
example str = APExample str (return ())

-- * test and combinators

test :: String -> ArgParser ()
test str = APTest str [] (return ())

eulerCharacteristic :: ArgParser a -> Int -> ArgParser a
eulerCharacteristic (APTest str tests child) χ =
    APTest str (EulerCharacteristic χ : tests) child
eulerCharacteristic _ _ = error "Impossible!"

-- * Tools for handeling ArgParsers

-- | Apply arguments to an ArgParser

argMap ::
    [(Maybe String,  OVal)]     -- ^ arguments
    -> ArgParser a              -- ^ ArgParser to apply them to
    -> (Maybe a, [String])      -- ^ (result, error messages)
argMap args = argMap2 unnamedArgs (Map.fromList namedArgs) where
    unnamedArgs = map snd $ filter (isNothing . fst) args
    namedArgs   = map (\(a,b) -> (fromJust a, b)) $ filter (isJust . fst) args

argMap2 :: [OVal] -> Map.Map String OVal -> ArgParser a -> (Maybe a, [String])
argMap2 uArgs nArgs (APBranch branches) =
    foldl1 merge solutions where
        solutions = map (argMap2 uArgs nArgs) branches
        merge :: (Maybe t, [t1]) -> (Maybe t, [t1]) -> (Maybe t, [t1])
        merge a@(Just _, []) _ = a
        merge _ b@(Just _, []) = b
        merge a@(Just _, _) _ = a
        merge (Nothing, _)  a = a
argMap2 unnamedArgs namedArgs (AP name fallback _ f) =
    case Map.lookup name namedArgs of
        Just a -> argMap2
            unnamedArgs
            (Map.delete name namedArgs)
            (f a)
        Nothing -> case unnamedArgs of
            x:xs -> argMap2 xs namedArgs (f x)
            []   -> case fallback of
                Just b  -> argMap2 [] namedArgs (f b)
                Nothing -> (Nothing, ["No value and no default for argument " ++ name])
argMap2 a b (APTerminator val) =
    (Just val, ["unused arguments" | not (null a && Map.null b)])
argMap2 a b (APFailIf testval err child) =
    if testval
    then (Nothing, [err])
    else argMap2 a b child
argMap2 a b (APExample _ child) = argMap2 a b child
argMap2 a b (APTest _ _ child) = argMap2 a b child
