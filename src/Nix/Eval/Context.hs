-- | String context construction, queries, and extraction.
--
-- Nix strings carry invisible metadata ("context") tracking which
-- store paths they reference.  When a derivation is built, its
-- environment strings' contexts are collected into @drvInputDrvs@ and
-- @drvInputSrcs@.  This module provides the pure helpers for building
-- and inspecting that context.
module Nix.Eval.Context
  ( -- * Construction
    plainContext,
    drvOutputContext,
    allOutputsContext,

    -- * Queries
    contextIsEmpty,
    firstContextEncoded,

    -- * Extraction (for derivation building)
    extractInputSrcs,
    extractInputDrvs,
    extractAllOutputRefs,

    -- * String operations with context
    appendStrings,
    concatStrings,
  )
where

import Data.Foldable (minimumBy)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Eval.Types (StringContext (..), StringContextElement (..))
import Nix.Store.Path (StorePath (spHash, spName))

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

-- | Context for a plain store path reference (inputSrcs).
plainContext :: StorePath -> StringContext
plainContext sp = StringContext (Set.singleton (SCPlain sp))

-- | Context for a derivation output reference (inputDrvs).
drvOutputContext :: StorePath -> Text -> StringContext
drvOutputContext sp outputName = StringContext (Set.singleton (SCDrvOutput sp outputName))

-- | Context for all outputs of a derivation (drvPath itself).
allOutputsContext :: StorePath -> StringContext
allOutputsContext sp = StringContext (Set.singleton (SCAllOutputs sp))

-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

-- | Check whether a string context is empty (no store path references).
contextIsEmpty :: StringContext -> Bool
contextIsEmpty (StringContext s) = Set.null s

-- | The element of a context upstream names first, in the encoding it
-- prints (@NixStringContextElem::to_string@ at 2.24.9): a plain path as
-- its base name, all outputs of a derivation as @=@ and the .drv's, one
-- output as @!output!@ and the .drv's.  Upstream keeps a context in a
-- @std::set@ of a variant ordered plain, all outputs, one output, which
-- is not this type's constructor order, so that order is searched here.
firstContextEncoded :: StringContext -> Maybe Text
firstContextEncoded (StringContext elems) =
  encoded . minimumBy (comparing upstreamOrder) <$> NE.nonEmpty (Set.toList elems)
  where
    upstreamOrder element = case element of
      SCPlain sp -> (0 :: Int, sp, T.empty)
      SCAllOutputs sp -> (1, sp, T.empty)
      SCDrvOutput sp output -> (2, sp, output)
    encoded element = case element of
      SCPlain sp -> baseName sp
      SCAllOutputs sp -> "=" <> baseName sp
      SCDrvOutput sp output -> "!" <> output <> "!" <> baseName sp
    baseName sp = spHash sp <> "-" <> spName sp

-- ---------------------------------------------------------------------------
-- Extraction
-- ---------------------------------------------------------------------------

-- | Extract plain store path references from context (for drvInputSrcs).
extractInputSrcs :: StringContext -> [StorePath]
extractInputSrcs (StringContext s) =
  [sp | SCPlain sp <- Set.toList s]

-- | Extract derivation output references from context (for drvInputDrvs).
-- Groups by derivation store path, collecting output names.
extractInputDrvs :: StringContext -> Map StorePath [Text]
extractInputDrvs (StringContext s) =
  Map.fromListWith (++) [(sp, [outName]) | SCDrvOutput sp outName <- Set.toList s]

-- | Extract all-outputs (upstream DrvDeep) derivation references - the @.drv@
-- store paths only.  Unlike 'extractInputDrvs', the output names are not
-- carried by the context element; the caller reads the referenced derivation
-- to recover its full set of output names.
extractAllOutputRefs :: StringContext -> [StorePath]
extractAllOutputRefs (StringContext s) =
  [sp | SCAllOutputs sp <- Set.toList s]

-- ---------------------------------------------------------------------------
-- String operations with context
-- ---------------------------------------------------------------------------

-- | Append two strings, merging their contexts.
appendStrings :: Text -> StringContext -> Text -> StringContext -> (Text, StringContext)
appendStrings t1 ctx1 t2 ctx2 = (t1 <> t2, ctx1 <> ctx2)

-- | Concatenate multiple strings with contexts, merging all contexts.
concatStrings :: [(Text, StringContext)] -> (Text, StringContext)
concatStrings = foldl' merge ("", mempty)
  where
    merge (!accText, !accCtx) (t, ctx) = (accText <> t, accCtx <> ctx)
