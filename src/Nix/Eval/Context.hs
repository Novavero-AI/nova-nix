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

    -- * Extraction (for derivation building)
    extractInputSrcs,
    extractInputDrvs,

    -- * String operations with context
    appendStrings,
    concatStrings,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Nix.Eval.Types (StringContext (..), StringContextElement (..))
import Nix.Store.Path (StorePath)

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

-- ---------------------------------------------------------------------------
-- String operations with context
-- ---------------------------------------------------------------------------

-- | Append two strings, merging their contexts.
appendStrings :: Text -> StringContext -> Text -> StringContext -> (Text, StringContext)
appendStrings t1 ctx1 t2 ctx2 = (t1 <> t2, ctx1 <> ctx2)

-- | Concatenate multiple strings with contexts, merging all contexts.
concatStrings :: [(Text, StringContext)] -> (Text, StringContext)
concatStrings = foldl merge ("", mempty)
  where
    merge (!accText, !accCtx) (t, ctx) = (accText <> t, accCtx <> ctx)
