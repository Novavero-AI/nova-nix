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
        attrSetFromMap
          $ Map.fromList
          $
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

-- | URL schemes that keep a @NIX_PATH@ entry whole when @://@ follows
-- them: the allowlist in @EvalSettings::isPseudoUrl@,
-- src/libexpr/eval-settings.cc at Nix 2.24.9.
nixPathUrlSchemes :: [Text]
nixPathUrlSchemes = ["http", "https", "file", "channel", "git", "s3", "ssh"]

-- | Schemes that keep a @NIX_PATH@ entry whole whatever follows their
-- colon: @isPseudoUrl@'s @channel:@ test and @parseNixPath@'s @flake:@
-- test, both in src/libexpr/eval-settings.cc at Nix 2.24.9.
nixPathBareSchemes :: [Text]
nixPathBareSchemes = ["channel", "flake"]

-- | Split a @NIX_PATH@ string into entries the way upstream's
-- @EvalSettings::parseNixPath@ does (src/libexpr/eval-settings.cc at
-- Nix 2.24.9).  An entry runs to the first colon.  That colon stays in
-- the entry, which then extends through exactly one more colon-free
-- run, when the text between the last @=@ before the colon (or the
-- entry's start) and the colon is:
--
-- * one of 'nixPathBareSchemes', whatever follows the colon, so
--   @channel:nixos-24.11@ and @nixpkgs=channel:nixos-24.11@ stay whole;
-- * one of 'nixPathUrlSchemes' with @//@ after the colon, so
--   @nixpkgs=https://example.com/nixpkgs.tar.gz@ stays whole while an
--   unlisted @foo://x@ splits into @foo@ and @//x@, as it does upstream;
-- * a single ASCII letter with @/@ or @\\@ after the colon: a Windows
--   drive colon, so @C:\\x@, @C:/x@ and @nixpkgs=C:\\x@ stay whole.
--   Upstream has no such rule and splits every drive path at its letter
--   (nix-instantiate 2.33.2 reports @C:\\a@ as the entries @C@ and @\\a@).
--   The rule also keeps a one-letter scheme such as @C://x@ whole, which
--   upstream splits into @C@ and @//x@.
--
-- Only one further run is absorbed, so @https://example.com:8080/x@
-- splits after the host exactly as it does upstream.  Every other colon
-- separates: @/foo:/bar@ is two entries.  An empty entry is dropped:
-- @a::b@ is @a@ and @b@, @::x@ is @x@, @a:@ is @a@.  Upstream's parser
-- emits a leading or interior empty entry (never a trailing one), but
-- the entries are then round-tripped through a whitespace-separated
-- setting (initGC in src/libexpr/eval-gc.cc) that drops it, so
-- @builtins.nixPath@ never shows one.  That round trip also splits an
-- entry at a space, which is not reproduced here: a Windows path may
-- contain one, the same reason the drive rule above diverges.
splitNixPath :: Text -> [Text]
splitNixPath = filter (not . T.null) . go
  where
    go remaining
      | T.null remaining = []
      | otherwise =
          let (segment, rest) = T.break (== ':') remaining
           in case T.uncons rest of
                Nothing -> [segment]
                Just (_, afterColon)
                  | keepsColon (T.takeWhileEnd (/= '=') segment) afterColon ->
                      let (absorbed, afterEntry) = T.break (== ':') afterColon
                       in T.concat [segment, ":", absorbed] : go (T.drop 1 afterEntry)
                  | otherwise -> segment : go afterColon
    keepsColon scheme afterColon
      | scheme `elem` nixPathBareSchemes = True
      | scheme `elem` nixPathUrlSchemes = T.isPrefixOf "//" afterColon
      | otherwise = isDriveLetter scheme && startsWithPathSep afterColon
    isDriveLetter scheme = case T.uncons scheme of
      Just (letter, afterLetter) -> T.null afterLetter && (isAsciiUpper letter || isAsciiLower letter)
      Nothing -> False
    startsWithPathSep t = case T.uncons t of
      Just (c, _) -> c == '/' || c == '\\'
      Nothing -> False
