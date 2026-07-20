-- | Built-in function environment for the Nix evaluator.
--
-- Every Nix expression has access to a @builtins@ attribute set containing
-- ~100 functions.  This module assembles the initial 'Env' from the
-- central registry in "Nix.Eval" and adds standard constants
-- (@true@, @false@, @null@, @storeDir@, @currentTime@,
-- @currentSystem@, etc.).
module Nix.Builtins
  ( -- * Builtin registration
    builtinEnv,
    builtinEnvWithScope,

    -- * NIX_PATH parsing
    parseNixPath,
    splitNixPath,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.Ptr (nullPtr)
import Nix.Eval (Env (..), NixValue (..), Thunk (..), attrSetFromMap, builtinNames, currentSystemStr, evaluated)
import Nix.Eval.Types (clistFromThunks, mkStr, newCEnv, thunkToCPtr)
import Nix.Store.Path (defaultStoreDirText)

-- | The initial environment containing all builtins.
--
-- Real Nix exposes a subset of builtins at the top level without
-- the @builtins.@ prefix.  These are the functions most commonly
-- used unqualified in nixpkgs and user code.
--
-- @currentTime@ is an integer constant (seconds since epoch),
-- passed in at startup.  In tests, pass @0@.
--
-- @searchPaths@ populates @builtins.nixPath@.  Parsed from @NIX_PATH@
-- by 'parseNixPath'.  In tests, pass @[]@.
builtinEnv :: Int64 -> [Thunk] -> Env
builtinEnv timestamp searchPaths =
  let scope =
        attrSetFromMap $
          Map.fromList $
            -- Values
            [ ("true", evaluated (VBool True)),
              ("false", evaluated (VBool False)),
              ("null", evaluated VNull),
              ("builtins", evaluated (builtinsAttrSet timestamp searchPaths)),
              -- Search path support: <name> desugars to __findFile __nixPath "name"
              -- (matching C++ Nix's parser desugaring).
              ("__findFile", evaluated (VBuiltin "findFile" [])),
              ("__nixPath", evaluated (VList (clistFromThunks (map thunkToCPtr searchPaths))))
            ]
              -- Top-level builtin functions (available without builtins. prefix)
              ++ map topLevelBuiltin topLevelBuiltinNames
   in newCEnv nullPtr 0 (Just scope) Nothing nullPtr 0

-- | Builtins exposed at the top level (without @builtins.@ prefix).
-- This matches real Nix - nixpkgs uses these unqualified everywhere.
-- Exactly upstream's unprefixed surface: fetchurl and toFile are
-- deliberately NOT here (upstream exposes them only under @builtins.@,
-- and nixpkgs relies on @with pkgs; fetchurl@ binding pkgs.fetchurl).
topLevelBuiltinNames :: [Text]
topLevelBuiltinNames =
  [ "abort",
    "baseNameOf",
    "break",
    "derivation",
    "derivationStrict",
    "dirOf",
    "fetchGit",
    "fetchTarball",
    "fromTOML",
    "import",
    "isNull",
    "map",
    "placeholder",
    "removeAttrs",
    "scopedImport",
    "throw",
    "toString"
  ]

-- | Create a top-level binding for a builtin function.
topLevelBuiltin :: Text -> (Text, Thunk)
topLevelBuiltin name = (name, evaluated (VBuiltin name []))

-- | Like 'builtinEnv' but with additional scope bindings overlaid on
-- the top-level environment.  Used by @scopedImport@.
builtinEnvWithScope :: Int64 -> [Thunk] -> [(Text, Thunk)] -> Env
builtinEnvWithScope timestamp searchPaths scope =
  let base = builtinEnv timestamp searchPaths
      scopeMap = Map.fromList scope
   in newCEnv nullPtr 0 (Just (attrSetFromMap scopeMap)) (Just base) nullPtr 0

-- | The @builtins@ attribute set, derived from the central registry.
builtinsAttrSet :: Int64 -> [Thunk] -> NixValue
builtinsAttrSet timestamp searchPaths =
  VAttrs $ attrSetFromMap $ Map.union builtinEntries (standardEntries timestamp searchPaths)
  where
    builtinEntries =
      Map.fromList [(name, evaluated (VBuiltin name [])) | name <- builtinNames]

standardEntries :: Int64 -> [Thunk] -> Map.Map Text Thunk
standardEntries timestamp searchPaths =
  Map.fromList
    [ ("true", evaluated (VBool True)),
      ("false", evaluated (VBool False)),
      ("null", evaluated VNull),
      -- Canonical, not platform: eval-visible store paths carry the
      -- /nix/store spelling on every platform, and storeDir must agree
      -- with them (upstream returns the store dir its paths are under).
      ("storeDir", evaluated (mkStr defaultStoreDirText)),
      ("nixVersion", evaluated (mkStr "2.24.0")),
      ("langVersion", evaluated (VInt 6)),
      ("nixPath", evaluated (VList (clistFromThunks (map thunkToCPtr searchPaths)))),
      ("currentTime", evaluated (VInt timestamp)),
      ("currentSystem", evaluated (mkStr currentSystemStr))
    ]

-- ---------------------------------------------------------------------------
-- NIX_PATH parsing
-- ---------------------------------------------------------------------------

-- | Parse a @NIX_PATH@-formatted string into a list of search path entry
-- thunks.  Each entry becomes a @{ prefix, path }@ attrset.
--
-- Format: colon-separated entries, each either @name=path@ or plain @path@.
-- A plain path gets an empty prefix (matching real Nix behaviour).
--
-- >>> parseNixPath "nixpkgs=/home/user/nixpkgs:custom=/opt/custom"
-- [Evaluated (VAttrs {"prefix": "nixpkgs", "path": "/home/user/nixpkgs"}), ...]
parseNixPath :: Text -> [Thunk]
parseNixPath raw
  | T.null raw = []
  | otherwise = map parseEntry (splitNixPath raw)
  where
    parseEntry entry =
      let (prefix, path) = case T.breakOn "=" entry of
            (before, after)
              | T.null after -> ("", before)
              | otherwise -> (before, T.drop 1 after)
       in evaluated
            ( VAttrs
                ( attrSetFromMap $
                    Map.fromList
                      [ ("prefix", evaluated (mkStr prefix)),
                        ("path", evaluated (mkStr path))
                      ]
                )
            )

-- | Split a NIX_PATH string on colon separators.  A colon stays part of
-- its entry, rather than separating, in exactly two shapes:
--
-- * a URL colon - followed by @//@ - so an entry like
--   @nixpkgs=https://example.com/nixpkgs.tar.gz@ stays whole (upstream's
--   NIX_PATH parser keeps pseudo-URL entries whole);
-- * a Windows drive colon - preceded by a single ASCII letter that opens
--   the entry or follows @=@, and followed by @\\@ or @/@ - so @C:\\x@,
--   @C:/x@, and @nixpkgs=C:\\x@ stay whole.
--
-- Every other colon separates, so a Unix-style list of absolute paths
-- (@/foo:/bar@) splits at each colon: @/foo@ does not end in a drive
-- letter, and a lone @/@ after the colon is not a URL.
splitNixPath :: Text -> [Text]
splitNixPath = go []
  where
    -- Accumulates reversed chunks and concatenates once per entry, so a
    -- long entry costs O(n) instead of the O(n^2) of per-character snoc.
    go !chunks remaining =
      let (chunk, rest) = T.break (== ':') remaining
       in case T.uncons rest of
            Nothing ->
              let entry = T.concat (reverse (chunk : chunks))
               in [entry | not (T.null entry)]
            Just (_, afterColon)
              | keepsColon (null chunks) chunk afterColon ->
                  go (T.take 1 afterColon : ":" : chunk : chunks) (T.drop 1 afterColon)
              | otherwise ->
                  T.concat (reverse (chunk : chunks)) : go [] afterColon
    -- Whether the colon between chunk and afterColon is a URL or drive
    -- colon (the two shapes above).  atEntryStart says chunk opens its
    -- entry (nothing absorbed before it), so a lone letter can only be a
    -- drive letter there.
    keepsColon atEntryStart chunk afterColon
      | T.isPrefixOf "//" afterColon = True
      | startsWithPathSep afterColon = endsInDriveLetter atEntryStart chunk
      | otherwise = False
    startsWithPathSep t = case T.uncons t of
      Just (c, _) -> c == '/' || c == '\\'
      Nothing -> False
    endsInDriveLetter atEntryStart chunk = case T.unsnoc chunk of
      Just (beforeLetter, letter) ->
        (isAsciiUpper letter || isAsciiLower letter)
          && (T.isSuffixOf "=" beforeLetter || (atEntryStart && T.null beforeLetter))
      Nothing -> False
